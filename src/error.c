#include "error.h"
#include "parser.h"
#include "tmp.h"

error_arg_t error_arg_none(void) {
    return (error_arg_t){.type=ERROR_ARG_TYPE_NONE, .token=nil_token, .node_or_null=NULL};
}

error_arg_t error_arg_token(token_t token) {
    return (error_arg_t){.type=ERROR_ARG_TYPE_TOKEN, .token=token, .node_or_null=NULL};
}

error_arg_t error_arg_node(ast_node_t *node) {
    return (error_arg_t){.type=ERROR_ARG_TYPE_NODE, .token=nil_token, .node_or_null=node};
}

error_arg_t error_arg_sz(size_t sz) {
    return (error_arg_t){.type=ERROR_ARG_TYPE_SIZE, .token=nil_token, .node_or_null=NULL, .size=sz};
}

error_arg_t error_arg_type(type_t type) {
    return (error_arg_t){.type=ERROR_ARG_TYPE_TYPE, .token=nil_token, .node_or_null=NULL, .size=0, .type_type=type};
}

static texloc_t error_arg_loc(error_arg_t arg) {
    switch (arg.type) {
    case ERROR_ARG_TYPE_TYPE:
    case ERROR_ARG_TYPE_SIZE:
    case ERROR_ARG_TYPE_NONE: return (texloc_t){.column=0, .filepath=lit2str("<none>"), .line=0};
    case ERROR_ARG_TYPE_NODE: {
        if (arg.node_or_null) {
            return arg.node_or_null->start.loc;
        }

        return (texloc_t){.column=0, .filepath=lit2str("<none>"), .line=0};
    }
    case ERROR_ARG_TYPE_TOKEN: return arg.token.loc;
    }
}

static string_view_t get_line(string_view_t source, string_view_t somewhere_in_source) {
    if (somewhere_in_source.data >= source.data + source.length || somewhere_in_source.data < source.data) {
        return lit2sv("");
    }

    char *s = (char*)somewhere_in_source.data;
    until (*s == '\n' || s == source.data)  {
        --s;
    }

    char *e = (char*)somewhere_in_source.data;
    until (*e == '\n' || *e == '\0') {
        ++e;
    }

    string_view_t view = {.length = e - s, .data = s};
    return view;
}

static string_t get_source_snippet(error_arg_t arg, arena_t *arena) {
    string_view_t source = lit2sv("");
    string_view_t view = source;
    size_t column_hint = 0;
    switch (arg.type) {
    case ERROR_ARG_TYPE_SIZE: break;
    case ERROR_ARG_TYPE_NONE: break;
    case ERROR_ARG_TYPE_TYPE: break;
    case ERROR_ARG_TYPE_TOKEN: {
        source = arg.token.source;
        view = arg.token.view;
        column_hint = arg.token.loc.column;
        break;
    }
    case ERROR_ARG_TYPE_NODE: {
        if (arg.node_or_null) {
            source = arg.node_or_null->start.source;
            view = arg.node_or_null->start.view;
            column_hint = arg.node_or_null->start.loc.column;
        }
        break;
    }
    }

    tmp_arena_t *tmp_arena = allocator_borrow();
    string_builder_t sb = {.allocator=tmp_arena->allocator};

    string_view_t source_line = get_line(source, view);
    sb_add_format(&sb, "%.*s\n", (int)source_line.length, source_line.data);

    for (size_t i = 0; i < source_line.length; ++i) {
        if (i == column_hint) {
            sb_add_char(&sb, '^');
        } else {
            sb_add_char(&sb, ' ');
        }
    }

    if (column_hint == source_line.length) {
        sb_add_char(&sb, '^');
    }

    sb_add_cstr(&sb, "\n");

    string_t result = sb_render(&sb, arena);

    allocator_return(tmp_arena);

    return result;
}

static size_t find_next_char_index(string_t s, size_t starting_index, char target) {
    for (size_t i = starting_index; i < s.length; ++i) {
        if (s.cstr[i] == target) return i;
    }

    return s.length;
}

static cstr_t tokentype2string(token_type_t token_type) {
    switch (token_type) {
        case TOKEN_IF: return "if";
        case TOKEN_UNLESS: return "unless";
        case TOKEN_UNTIL: return "until";
        case TOKEN_WHILE: return "while";
        case TOKEN_DO: return "do";
        case TOKEN_FOR: return "for";

        default: return "<todo>";
    }
}


static string_t error_format(string_t message_format, typedatas_t *tds, error_arg_t *args, arena_t *arena) {
    tmp_arena_t *tmp = allocator_borrow();
    string_builder_t sb = {.allocator=tmp->allocator};

    for (size_t i = 0; i < message_format.length; ++i) {
        char c = message_format.cstr[i];
        if (c != '$') {
            sb_add_char(&sb, c);
        } else {
            ++i; // skip '$'

            size_t dot_index = find_next_char_index(message_format, i, '.');
            ASSERT(dot_index < message_format.length, "must");

            string_view_t arg_index_sv = {.data=message_format.cstr+i, .length=dot_index-i};
            string_t arg_index_str = sv2string(arg_index_sv, tmp->allocator);
            size_t arg_index = string2size(arg_index_str);

            i = dot_index + 1;

            size_t end_index = find_next_char_index(message_format, i, '$');
            ASSERT(end_index < message_format.length, "must be surrounded by '$'s");
            string_view_t field_sv = {.data=message_format.cstr+i, .length=end_index-i};

            i = end_index;

            error_arg_t arg = args[arg_index];

            if (sv_eq(field_sv, lit2sv("kind"))) {
                switch (arg.type) {
                case ERROR_ARG_TYPE_TOKEN: {
                    cstr_t token_type_str = tokentype2string(arg.token.type);
                    sb_add_cstr(&sb, token_type_str);
                    break;
                }
                case ERROR_ARG_TYPE_SIZE: break;
                case ERROR_ARG_TYPE_NODE: break;
                case ERROR_ARG_TYPE_NONE: break;
                case ERROR_ARG_TYPE_TYPE: break;
                }
            } else if (sv_eq(field_sv, lit2sv(""))) {
                switch (arg.type) {
                case ERROR_ARG_TYPE_TOKEN: sb_add_format(&sb, "%.*s", arg.token.view.length, arg.token.view.data); break;
                case ERROR_ARG_TYPE_SIZE: sb_add_format(&sb,"%llu", arg.size); break;
                case ERROR_ARG_TYPE_NODE: {
                    if (arg.node_or_null) {
                        string_view_t sv;
                        sv.data = arg.node_or_null->start.view.data;
                        sv.length = arg.node_or_null->end.view.data + arg.node_or_null->end.view.length - sv.data;
                        sb_add_format(&sb, "%.*s", sv.length, sv.data);
                    }
                    break;
                }
                case ERROR_ARG_TYPE_NONE: break;
                case ERROR_ARG_TYPE_TYPE: {
                    tmp_arena_t *tmp = allocator_borrow();
                    string_t s = type_to_string(*tds, arg.type_type, tmp->allocator);

                    sb_add_format(&sb, "%s", s.cstr);

                    allocator_return(tmp);
                    break;
                }
                }
            } else {
                UNREACHABLE();
            }
        }
    }

    string_t result = sb_render(&sb, arena);

    allocator_return(tmp);
    
    return result;
}

string_t error2richstring(ast_t *ast, error_t error, arena_t *arena) {
    tmp_arena_t *tmp = allocator_borrow();

    string_builder_t sb = {.allocator=tmp->allocator};

    string_t message = error_format(error.msg, &ast->type_set.types, error.args, tmp->allocator);

    for (size_t i = 0; i < error.show_line_count; ++i) {
        size_t arg_index = error.show_code_lines[i];
        error_arg_t arg = error.args[arg_index];
        string_t snippet = get_source_snippet(arg, tmp->allocator);

        texloc_t loc = error_arg_loc(arg);

        cstr_t error_level = "";
        switch (error.level) {
        case ERROR_SOURCE_PARSEREX:
        case ERROR_SOURCE_PARSER: error_level = "syntax"; break;
        case ERROR_SOURCE_ANALYSIS: error_level = "semantic"; break;
        case ERROR_SOURCE_CODEGEN: error_level = "codegen"; break;
        }

        if (i == 0) {
            sb_add_format(&sb, "%s error: %s:%llu:%llu: %s\n", error_level, loc.filepath.cstr, loc.line+1, loc.column+1, message.cstr);
        } else {
            sb_add_format(&sb, ">> %s:%llu:%llu:\n", loc.filepath.cstr, loc.line+1, loc.column+1);
        }

        sb_add_cstr(&sb, snippet.cstr);
        sb_add_cstr(&sb, "\n");
    }

    string_t result = sb_render(&sb, arena);

    allocator_return(tmp);

    return result;
}