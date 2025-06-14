#!/usr/bin/env -S awk --traditional --lint --exec
BEGIN {
    SUBSEP = "@"
    if (ARGC != 2) {
        print "Usage: c.awk program.c"
        exit 1
    }
    for (ARGNO = 1; ARGNO < ARGC; ARGNO++) {
        ARG = ARGV[ARGNO]
        delete ARGV[ARGNO]
    }
    INPUT_FILE = ARG
    ASSEMBLY_FILE = INPUT_FILE
    sub(/\.c$/, ".s", ASSEMBLY_FILE)
    SOURCE = ""
    while ((getline LINE < INPUT_FILE) > 0) {
        SOURCE = SOURCE LINE "\n"
    }

    lex(S, SOURCE)
    for (I = 1; I <= S["num_tokens"]; I++) {
        print S["tokens", I, "type"], S["tokens", I, "value"]
    }
    S["input_file"] = INPUT_FILE
    close(INPUT_FILE)

    parse(S)
    print_ast(S)
}


function lex(s, source,   line, col, i, token_index, symbol, found)
{
    token_index = 1
    split("auto break case char const continue default do double else enum extern float for goto if int long register return short signed sizeof static struct switch typedef union unsigned void volatile while >>= <<= += -= *= /= %= &= ^= |= >> << ++ -- -> && || <= >= == != ; | { } , = ( ) [ ] . & ! ~ - + * / % < > ^ | ?", SYMBOLS, FS)
    line = 1
    col = 1
    while (source) {
        match(source, /^[ \t]*/)
        if (RLENGTH >= 1) {
            col += RLENGTH
            source = substr(source, RLENGTH + 1)
            continue
        }
        if (substr(source, 1, 1) == "\n") {
            line += 1
            col = 1
            if (length(source) == 1) {
                source = ""
            } else {
                source = substr(source, 2)
            }
            continue
        }

        found = 0
        for (i in SYMBOLS) {
            symbol = SYMBOLS[i]
            if (length(source) >= length(symbol) && substr(source, 1, length(symbol)) == symbol) {
                s["tokens", token_index, "type"] = symbol
                s["tokens", token_index, "value"] = symbol
                s["tokens", token_index, "line"] = line
                s["tokens", token_index, "col"] = col
                token_index++
                source = substr(source, length(symbol) + 1)
                found = 1
                break
            }
        }
        if (found) {
            continue
        }

        match(source, /^[a-zA-Z][a-zA-Z0-9_]**/)
        if (RLENGTH >= 1) {
            s["tokens", token_index, "type"] = "id"
            s["tokens", token_index, "value"] = substr(source, 1, RLENGTH)
            s["tokens", token_index, "line"] = line
            s["tokens", token_index, "col"] = col
            token_index++
            col += RLENGTH
            source = substr(source, RLENGTH + 1)
            continue
        }

        match(source, /^[0-9]+*/)
        if (RLENGTH >= 1) {
            s["tokens", token_index, "type"] = "int"
            s["tokens", token_index, "value"] = substr(source, 1, RLENGTH) + 0
            s["tokens", token_index, "line"] = line
            s["tokens", token_index, "col"] = col
            token_index++
            col += RLENGTH
            source = substr(source, RLENGTH + 1)
            continue
        }

        printf "Warning: Skipping unrecognized character at %s:%d:%d\n", INPUT_FILE, line, col
        source = substr(source, 2)
    }
    s["tokens", token_index, "type"] = "eof"
    s["tokens", token_index, "value"] = ""
    s["tokens", token_index, "line"] = line
    s["tokens", token_index, "col"] = col
    s["num_tokens"] = token_index
}

function parse(s,      program) {
    s["parser_state", "token_index"] = 1
    s["parser_state", "ast_index"] = 1

    program = start_node(s, "program")
    parse_function_decl(s)
    end_node(s, program)
}

function parse_function_decl(s,     a) {
    a = start_node(s, "function_decl")
    parser_expect(s, "int")
    set_node_attr(s, a, "name", parser_expect(s, "id"))
    parser_expect(s, "(")
    parser_expect(s, ")")
    parser_expect(s, "{")
    set_node_attr(s, a, "body", parse_statment(s))
    parser_expect(s, "}")
    end_node(s, a)
    return a
}

function parse_statment(s,     a) {
    a = start_node(s, "return")
    parser_expect(s, "return")
    set_node_attr(s, a, "expr", parse_expr(s))
    parser_expect(s, ";")
    end_node(s, a)
    return a
}

function parse_expr(s,     a) {
    a = start_node(s, "int")
    set_node_attr(s, a, "value", parser_expect(s, "int"))
    end_node(s, a)
    return a
}

function parser_expect(s, token_type,   value) {
    if (parser_peek(s) != token_type) {
        parser_error(s, sprintf("Expected %s, found %s", token_type, parser_peek(s)))
    }
    return parser_advance(s)
}

function parser_peek(s) {
    return s["tokens", s["parser_state", "token_index"], "type"]
}

function parser_advance(s,    value) {
    value = s["tokens", s["parser_state", "token_index"], "value"]
    s["parser_state", "token_index"]++
    return value
}

function parser_error(s, error_message) {
    printf("%s:%d:%d: %s\n", s["input_file"], s["tokens", s["parser_state", "token_index"], "line"], s["tokens", s["parser_state", "token_index"], "col"], error_message)
    exit 1
}

function start_node(s, node_type,       ast_index) {
    s["ast", s["parser_state", "ast_index"], "type"] = node_type
    s["ast", s["parser_state", "ast_index"], "line"] = s["tokens", s["parser_state", "token_index"], "line"]
    s["ast", s["parser_state", "ast_index"], "col"] = s["tokens", s["parser_state", "token_index"], "col"]
    s["ast", s["parser_state", "ast_index"], "attrs"] = ""
    ast_index = s["parser_state", "ast_index"]
    s["parser_state", "ast_index"]++
    return ast_index
}

function end_node(s, ast_index) {
    s["ast", ast_index, "end"] = s["parser_state", "ast_index"] - 1
}

function set_node_attr(s, ast_index, key, value) {
    s["ast", ast_index, key] = value
    if (s["ast", ast_index, "attrs"]) {
        s["ast", ast_index, "attrs"] = s["ast", ast_index, "attrs"] ":" key
    } else {
        s["ast", ast_index, "attrs"] = key
    }

}

function get_node_attr(s, ast_index, key) {
    return s["ast", ast_index, key]
}

function print_ast(s) {
    print_ast_inner(s, 1, 0)
}

function print_ast_inner(s, ast_index, indent,    curr, end, i, attrs) {
    for (i = 0; i < indent; i++) {
        printf " "
    }
    printf "%s", s["ast", ast_index, "type"]
    split(s["ast", ast_index, "attrs"], attrs, ":")
    for (i in attrs) {
        printf " %s=%s", attrs[i], get_node_attr(s, ast_index, attrs[i])
    }
    printf "\n"
    end = s["ast", ast_index, "end"]
    if (!end) {
        print "Forgot to end this node"
        exit 1
    }
    curr = ast_index + 1
    while (curr <= end) {
        curr = print_ast_inner(s, curr, indent + 2) + 1
    }
    return end
}

