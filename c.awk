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
    NUM_TOKENS = lex(SOURCE, TOKENS)
    for (I = 1; I <= NUM_TOKENS; I++) {
        print TOKENS[I, "type"]
    }
    close(INPUT_FILE)

    parse(TOKENS, NUM_TOKENS, AST)
    print_ast(AST)
}


function lex(source, tokens,    i, token_index, symbol, found)
{
    token_index = 1
    split("auto break case char const continue default do double else enum extern float for goto if int long register return short signed sizeof static struct switch typedef union unsigned void volatile while >>= <<= += -= *= /= %= &= ^= |= >> << ++ -- -> && || <= >= == != ; | { } , = ( ) [ ] . & ! ~ - + * / % < > ^ | ?", SYMBOLS, FS)
    LINE = 1
    col = 1
    while (source) {
        match(source, /^[ \t]*/)
        if (RLENGTH >= 1) {
            col += RLENGTH
            source = substr(source, RLENGTH + 1)
            continue
        }
        if (substr(source, 1, 1) == "\n") {
            LINE += 1
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
                tokens[token_index, "type"] = symbol
                tokens[token_index, "value"] = symbol
                tokens[token_index, "line"] = LINE
                tokens[token_index, "col"] = col
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
            tokens[token_index, "type"] = "id"
            tokens[token_index, "value"] = substr(source, 1)
            tokens[token_index, "line"] = LINE
            tokens[token_index, "col"] = col
            token_index++
            col += RLENGTH
            source = substr(source, RLENGTH + 1)
            continue
        }

        match(source, /^[0-9]+*/)
        if (RLENGTH >= 1) {
            tokens[token_index, "type"] = symbol
            tokens[token_index, "value"] = substr(source, 1, RLENGTH) + 0
            tokens[token_index, "line"] = LINE
            tokens[token_index, "col"] = col
            token_index++
            col += RLENGTH
            source = substr(source, RLENGTH + 1)
            continue
        }

        printf "Warning: Skipping unrecognized character at %s:%d:%d\n", INPUT_FILE, LINE, col
        source = substr(source, 2)
    }
    return token_index - 1
}

function parse(tokens, num_tokens, ast,     parser_state) {
    parser_state["token_index"] = 1
    parser_state["ast_index"] = 1
    parser_state["num_tokens"] = num_tokens

    program = start_node("program", tokens, num_tokens, ast, parser_state)
    function_decl = start_node("function_decl", tokens, num_tokens, ast, parser_state)
    first_statement = start_node("first_statement", tokens, num_tokens, ast, parser_state)
    end_node(first_statement, ast, parser_state)
    second_statement = start_node("second_statement", tokens, num_tokens, ast, parser_state)
    end_node(second_statement, ast, parser_state)
    end_node(function_decl, ast, parser_state)
    end_node(program, ast, parser_state)
}

function start_node(node_type, tokens, num_tokens, ast, parser_state) {
    ast[parser_state["ast_index"], "type"] = node_type
    ast[parser_state["ast_index"], "line"] = tokens[parser_state["token_index"], "line"]
    ast[parser_state["ast_index"], "col"] = tokens[parser_state["token_index"], "col"]
    ast_index = parser_state["ast_index"]
    parser_state["ast_index"]++
    return ast_index
}

function end_node(ast_index, ast, parser_state) {
    ast[ast_index, "end"] = parser_state["ast_index"] - 1
}

function print_ast(ast) {
    print_ast_inner(ast, 1, 0)
}

function print_ast_inner(ast, ast_index, indent,    curr, end, i) {
    for (i = 0; i < indent; i++) {
        printf " "
    }
    print ast[ast_index, "type"]
    end = ast[ast_index, "end"]
    curr = ast_index + 1
    while (curr <= end) {
        curr = print_ast_inner(ast, curr, indent + 2) + 1
    }
    return end
}

