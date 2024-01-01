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
const JsonParseStateTag = enum { top, tokInString, tokBackslash, arrayDelimiter, objectKey, objectPairDelimiter, consumingBoolean, consumingDouble, consumingNull };
const JsonParseState = struct {
    // Stack: The previous states.
    tags: std.ArrayList(JsonParseStateTag),
    // Stack: The partial JSON values.
    // The main JsonValue passed to the parser (assumed to have no actual contents before the main parser) will own all these pointers (or their direct owners).
    values: std.ArrayList(*JsonValue),
    // Allocator for the stack and the outputted JsonValue.
    allocator: std.mem.Allocator,
    start: usize,
    end: usize,
    // Points into string.
    index: usize,
    // Unowned.
    string: []const u8,

    fn init(allocator: std.mem.Allocator, string: []const u8) JsonParseState {
        return JsonParseState{ .tags = std.ArrayList(JsonParseStateTag).init(allocator), .values = std.ArrayList(*JsonValue).init(allocator), .allocator = allocator, .start = 0, .end = 0, .index = 0, .string = string };
    }

    fn deinit(self: *JsonParseState) void {
        self.tags.deinit();
        self.values.deinit();
    }

    fn parseOneChar(self: *JsonParseState, value: *JsonValue) (JsonParseError || std.mem.Allocator.Error)!void {
        const char = self.string[self.index];
        switch (self.tags.getLast()) {
            JsonParseStateTag.top => {
                const typeTag = JsonTypeTag.classifyByFirstChar(char);
                if (!typeTag) return JsonParseError.ConfusingDelimiter;
                self.start = self.index;
                switch (typeTag.?) {
                    JsonTypeTag.string => {
                        self.start += 1;
                        try self.tags.append(JsonParseStateTag.tokInString);
                    },
                    JsonTypeTag.double => {
                        try self.tags.append(JsonParseStateTag.consumingDouble);
                    },
                    JsonTypeTag.boolean => {
                        try self.tags.append(JsonParseStateTag.consumingBoolean);
                    },
                    JsonTypeTag.jsonNull => {
                        try self.tags.append(JsonParseStateTag.consumingNull);
                    },
                    JsonTypeTag.array => {
                        try self.tags.append(JsonParseStateTag.arrayDelimiter);
                        try self.tags.append(JsonParseStateTag.top);
                    },
                    JsonTypeTag.object => {
                        try self.tags.append(JsonParseStateTag.objectKey);
                        try self.tags.append(JsonParseStateTag.top);
                    },
                }
            },
            JsonParseStateTag.tokInString => {
                switch (char) {
                    '\\' => {
                        self.tags.items[self.tags.items.len - 1] = JsonParseStateTag.tokBackslash;
                        self.end = self.index;
                    },
                    '"' => {
                        value.string.init(self.allocator);
                        try value.string.insertSlice(self.string[self.start .. self.index + 1]);
                    },
                    else => {},
                }
            },
            else => return JsonParseState.UnclosedStringError,
        }
        self.index += 1;
    }
};

const JsonValue = union(JsonTypeTag) {
    string: std.ArrayList(u8),
    double: f64,
    boolean: bool,
    jsonNull: enum { jsonNull },
    array: std.ArrayList(JsonValue),
    object: std.StringHashMap(JsonValue),

    // Parse a JSON string
    pub fn fromString(string: []const u8, allocator: std.mem.Allocator) JsonParseError!JsonValue {
        // Whinig: no lambda for just passing a continuation T_T
        var tokStack = std.ArrayList(JsonParseState).init(allocator);
        defer tokStack.deinit();
        tokStack.append(JsonParseState.top);
        var topLevel = JsonValue.jsonNull;
        for (string) |char| {
            var t = JsonTypeTag.classifyByFirstChar(char);
            if (!t) {
                return JsonParseError.ConfusingDelimiter;
            }
        }
        if (tokStack.items.len > 1 or tokStack.items[0] != JsonParseState.top) {
            return JsonParseError.ExpectedMore;
        }
        return topLevel;
    }
};
