const std = @import("std");

// TODO: consider https://zig.news/ityonemo/error-payloads-updated-367m
const JsonParseError = error{ LiteralParseError, UnclosedStringError, ArrayParseError, ObjectParseError, ConfusingDelimiter, ExpectedMore };

const JsonTypeTag = enum {
    string,
    double,
    boolean,
    jsonNull,
    array,
    object,

    // Classify the expected type based on a single character.
    pub fn classifyByFirstChar(char: u8) ?JsonTypeTag {
        switch (char) {
            '{' => JsonTypeTag.object,
            '[' => JsonTypeTag.array,
            '"' => JsonTypeTag.string,
            '-' => JsonTypeTag.double,
            '0'...'9' => JsonTypeTag.double,
            't' => JsonTypeTag.boolean,
            'f' => JsonTypeTag.boolean,
            'n' => JsonTypeTag.jsonNull,
            else => null,
        }
    }
};

// The main recusrive traversal for parsing JSON.
// Drives the baseline state.
const JsonParseStateTag = enum { top, arrayDelimiter, objectDelimiter };
const JsonParseState = struct {
    // Stack: The previous states.
    tags: std.ArrayList(JsonParseStateTag),
    // Stack: The partial JSON values.
    // The main JsonValue passed to the parser (assumed to have no actual contents before the main parser) will own all these pointers (or their direct owners).
    values: std.ArrayList(*JsonValue),
    // Stack: all the arrays we're traversing into.
    arrayStack: std.ArrayList(*std.ArrayList(JsonValue)),
    // Allocator for the stack and the outputted JsonValue.
    allocator: *std.mem.Allocator,
    // Points into string.
    index: usize,
    // Unowned.
    string: []const u8,
    // Tables for the allocated value.
    stringTable: *std.ArrayList(std.ArrayList(u8)),
    subItemTable: *std.ArrayList(std.ArrayList(JsonValue)),

    fn init(allocator: *std.mem.Allocator, string: []const u8, top_value: JsonValue, stringTable: *std.ArrayList(std.ArrayList(u8)), subItemTable: *std.ArrayList(std.ArrayList(JsonValue))) std.mem.Allocator.Error!JsonParseState {
        var tags = std.ArrayList(JsonParseStateTag).init(allocator);
        try tags.append(JsonParseState.top);
        var values = std.ArrayList(*JsonValue).init(allocator);
        try values.append(&top_value);
        return JsonParseState{ .tags = tags, .values = values, .arrayStack = std.ArrayList(*std.ArrayList(JsonValue)).init(), .allocator = allocator, .start = 0, .end = 0, .index = 0, .string = string, .stringTable = stringTable, .subItemTable = subItemTable };
    }

    fn deinit(self: *JsonParseState) void {
        self.tags.deinit();
        self.values.deinit();
        self.arrayStack.deinit();
    }

    fn consumeLiteralString(self: *JsonParseState, comptime literal: []const u8) JsonParseError!void {
        if (!std.mem.eql(u8, self.string[self.index..literal.len], literal)) {
            return JsonParseError.LiteralParseError;
        }
        self.index += literal.len;
    }

    fn consumeString(self: *JsonParseState) JsonParseError![]const u8 {
        var start_index = self.index;
        while (self.index < self.string.len and self.string[self.index] != '"') : (self.index += 1) {
            if (self.string[self.index] == '\\') {
                self.index += 1;
            }
        }
        if (self.index >= self.string.len) {
            return JsonParseError.UnclosedStringError;
        }
        self.index += 1;
        const found_string = if (start_index + 1 >= self.index - 2) {
            "";
        } else {
            self.string[start_index + 1 .. self.index - 2];
        };
        try self.stringTable.append(std.ArrayList(u8).init(self.allocator));
        try self.stringTable.getLast().insertSlice(0, found_string);
        return self.stringTable.getLast().items;
    }

    fn consumeDouble(self: *JsonParseState) JsonParseError!void {
        const DoubleTokState = enum { start, beforeDec, firstAfterDec, laterAfterDec, afterExp, afterExpSign, done };
        var state = DoubleTokState.start;
        var negate = false;
        // f64 saves a cast for the final multiplication; we do not allow decimal exporents.
        var exponent: ?f64 = null;
        var n_after_dec: f64 = 1;
        var acc: f64 = 0;
        // TODO: unroll into smaller loops? Or make an FSM macro?
        while (self.index < self.string.len and state != DoubleTokState.done) : (self.index += 1) {
            state = switch (state) {
                DoubleTokState.start => {
                    switch (self.string[self.index]) {
                        '-' => {
                            negate = true;
                            break DoubleTokState.beforeDec;
                        },
                        '0'...'9' => {
                            acc += @as(f64, self.string[self.index] - '0');
                            break DoubleTokState.beforeDec;
                        },
                        else => {
                            return JsonParseError.LiteralParseError;
                        },
                    }
                },
                DoubleTokState.beforeDec => {
                    switch (self.string[self.index]) {
                        '0'...'9' => {
                            acc *= 10;
                            acc += @as(f64, self.string[self.index] - '0');
                            break DoubleTokState.beforeDec;
                        },
                        '.' => {
                            break DoubleTokState.firstAfterDec;
                        },
                        else => {
                            break DoubleTokState.done;
                        },
                    }
                },
                DoubleTokState.firstAfterDec => {
                    switch (self.string[self.index]) {
                        '0'...'9' => {
                            n_after_dec *= 10;
                            acc += @as(f64, self.string[self.index] - '0') / n_after_dec;
                            break DoubleTokState.laterAfterDec;
                        },
                        else => {
                            return JsonParseError.LiteralParseError;
                        },
                    }
                },
                DoubleTokState.laterAfterDec => {
                    switch (self.string[self.index]) {
                        '0'...'9' => {
                            n_after_dec *= 10;
                            acc += @as(f64, self.string[self.index] - '0') / n_after_dec;
                            break DoubleTokState.laterAfterDec;
                        },
                        'e' | 'E' => {
                            exponent = 1;
                            break DoubleTokState.afterExp;
                        },
                        else => {
                            break DoubleTokState.done;
                        },
                    }
                },
                DoubleTokState.afterExp => {
                    switch (self.string[self.index]) {
                        '-' => {
                            exponent = -1;
                            break DoubleTokState.afterExpSign;
                        },
                        '0'...'9' => {
                            exponent += @as(f64, self.string[self.index] - '0');
                            break DoubleTokState.afterExpSign;
                        },
                        else => {
                            return JsonParseError.LiteralParseError;
                        },
                    }
                },
                DoubleTokState.afterExpSign => {
                    switch (self.string[self.index]) {
                        '0'...'9' => {
                            exponent *= 10;
                            exponent += @as(f64, self.string[self.index] - '0');
                            break DoubleTokState.afterExpSign;
                        },
                        else => {
                            return JsonParseError.done;
                        },
                    }
                },
            };
        }
        if (exponent) |e| {
            acc *= std.map.pow(f64, 10.0, e);
        }
        self.values.getLast().double = acc;
    }

    fn advanceOnce(self: *JsonParseState) (JsonParseError || std.mem.Allocator.Error)!void {
        const char = self.string[self.index];
        switch (self.tags.getLast()) {
            JsonParseStateTag.top => {
                const typeTag = JsonTypeTag.classifyByFirstChar(char);
                if (!typeTag) return JsonParseError.ConfusingDelimiter;
                self.start = self.index;
                switch (typeTag.?) {
                    JsonTypeTag.string => {
                        self.values.getLast().string = try self.consumeString();
                        self.tags.pop();
                    },
                    JsonTypeTag.double => {
                        try self.consumeDouble();
                        self.tags.pop();
                    },
                    JsonTypeTag.boolean => {
                        if (char == 't') {
                            try self.consumeLiteralString("true");
                            self.values.getLast().boolean = true;
                        } else {
                            try self.consumeLiteralString("false");
                            self.values.getLast().boolean = false;
                        }
                        self.tags.pop();
                    },
                    JsonTypeTag.jsonNull => {
                        try self.consumeLiteralString("null");
                        self.tags.pop();
                    },
                    JsonTypeTag.array => {
                        self.tags.items[self.tags.items.len - 1] = JsonParseStateTag.arrayDelimiter;
                        try self.tags.append(JsonParseStateTag.top);
                        self.values.getLast().array.init(self.allocator);
                        try self.values.getLast().array.append(JsonValue{.jsonNull});
                        try self.values.append(&self.values.getLast().array.getLast());
                        try self.subItemTable.append(std.ArrayList(JsonValue).init());
                        try self.arrayStack.append(&self.subItemTable.getLast());
                    },
                    JsonTypeTag.object => {
                        self.tags.items[self.tags.items.len - 1] = JsonParseStateTag.objectKey;
                        try self.tags.append(JsonParseStateTag.top);
                        self.values.getLast().object.init(self.allocator);
                        const str = try self.consumeString();
                        try self.subItemTable.append(std.ArrayList(JsonValue).init());
                        try self.subItemTable.getLast().append(JsonValue{ .jsonNull = {} });
                        try self.values.getLast().object.put(str, &self.subItemTable.getLast().getLast());
                        try self.values.append(&self.values.getLast().object.getPtr(str).?);
                        if (self.string[self.index] != ':') {
                            return JsonParseError.ConfusingDelimiter;
                        }
                        self.index += 1;
                    },
                    else => {
                        return JsonParseError.ConfusingDelimiter;
                    },
                }
            },
            JsonParseState.arrayDelimiter => {
                switch (char) {
                    ',' => {
                        try self.tags.append(JsonParseStateTag.top);
                        try self.arrayStack.getLast().append(JsonValue{.jsonNull});
                        try self.values.append(&self.arrayStack.getLast().getLast());
                    },
                    ']' => {
                        self.tags.pop();
                        self.values.pop();
                        const lastArray = self.arrayStack.pop();
                        self.values.getLast().array = lastArray.items;
                    },
                    else => {
                        return JsonParseError.ConfusingDelimiter;
                    },
                }
            },
            JsonParseState.objectDelimiter => {
                switch (char) {
                    ',' => {
                        try self.tags.append(JsonParseStateTag.top);
                        const str = try self.consumeString();
                        try self.subItemTable.append(std.ArrayList(JsonValue).init());
                        try self.subItemTable.getLast().append(JsonValue{ .jsonNull = {} });
                        try self.values.getLast().object.put(str, &self.subItemTable.getLast().getLast());
                        try self.values.append(&self.values.getLast().object.get(str).?);
                        if (self.string[self.index] != ':') {
                            return JsonParseError.ConfusingDelimiter;
                        }
                        self.index += 1;
                    },
                    '}' => {
                        self.tags.pop();
                        self.values.pop();
                    },
                    else => {
                        return JsonParseError.ConfusingDelimiter;
                    },
                }
            },
        } // close switch on the JsonParseState.
    }
};

// A JSON value. Unaware of the allocator used to produce, so inherently hard to modify.
// Use the AllocatedJsonValue for writing and the JsonValue for reading.
const JsonValue = union(JsonTypeTag) {
    string: []const u8,
    double: f64,
    boolean: bool,
    jsonNull: void,
    array: []const JsonValue,
    object: std.StringHashMap(*JsonValue),
};

// The mdifiable JSON value.
const AllocatedJsonValue = struct {
    root: JsonValue,
    allocator: std.mem.Allocator,
    stringTable: std.ArrayList(std.ArrayList(u8)),
    subItemTable: std.ArrayList(std.ArrayList(JsonValue)),

    // Parse a JSON string
    pub fn fromString(string: []const u8, allocator: std.mem.Allocator) (JsonParseError || std.mem.Allocator.Error)!AllocatedJsonValue {
        var new_root = JsonValue{.jsonNull};
        var state = try JsonParseState.init(allocator, string, new_root);
        defer state.deinit();
        while (state.tags.len > 0) {
            try state.advanceOnce();
        }
        return AllocatedJsonValue{ .root = new_root, .allocator = allocator };
    }

    pub fn deinit(self: *AllocatedJsonValue) void {
        for (self.subItemTable.items) |item| {
            switch (item) {
                .object => |o| {
                    o.deinit();
                },
                else => {},
            }
        }
        switch (self.root) {
            .object => |o| {
                o.deinit();
            },
            else => {},
        }
        self.stringTable.deinit();
        self.subItemTable.deinit();
    }
};

test "parses doubles" {}
