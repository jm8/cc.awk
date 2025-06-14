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
        print S["tokens", I, "type"]
    }
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
            s["tokens", token_index, "value"] = substr(source, 1)
            s["tokens", token_index, "line"] = line
            s["tokens", token_index, "col"] = col
            token_index++
            col += RLENGTH
            source = substr(source, RLENGTH + 1)
            continue
        }

        match(source, /^[0-9]+*/)
        if (RLENGTH >= 1) {
            s["tokens", token_index, "type"] = symbol
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
    return s["num_tokens"] = token_index - 1
}

function parse(s) {
    s["parser_state", "token_index"] = 1
    s["parser_state", "ast_index"] = 1

    program = start_node(s, "program")
    function_decl = start_node(s, "function_decl")
    first_statement = start_node(s, "first_statement")
    end_node(s, first_statement)
    second_statement = start_node(s, "second_statement")
    end_node(s, second_statement)
    end_node(s, function_decl)
    end_node(s, program)
}

function start_node(s, node_type,       ast_index) {
    s["ast", s["parser_state", "ast_index"], "type"] = node_type
    s["ast", s["parser_state", "ast_index"], "line"] = s["tokens", s["parser_state", "token_index"], "line"]
    s["ast", s["parser_state", "ast_index"], "col"] = s["tokens", s["parser_state", "token_index"], "col"]
    ast_index = s["parser_state", "ast_index"]
    s["parser_state", "ast_index"]++
    return ast_index
}

function end_node(s, ast_index) {
    s["ast", ast_index, "end"] = s["parser_state", "ast_index"] - 1
}

function print_ast(s) {
    print_ast_inner(s, 1, 0)
}

function print_ast_inner(s, ast_index, indent,    curr, end, i) {
    for (i = 0; i < indent; i++) {
        printf " "
    }
    print s["ast", ast_index, "type"]
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

