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

    S["input_file"] = INPUT_FILE
    lex(S, SOURCE)
    # for (I = 1; I <= S["num_tokens"]; I++) {
    #     print S["tokens", I, "type"], S["tokens", I, "value"]
    # }
    close(INPUT_FILE)

    parse(S)
    if (0) ast_print(S)
    lower(S)
    if (0) ir_print(S)

    codegen(S)
}

function list_append(s, key, value) {
    if (key in s && s[key]) {
        s[key] = s[key] ":" value
    } else {
        s[key] = value
    }
}

function list_get(s, key, out) {
    if (key in s && s[key]) {
        split(s[key], out, ":")
    } else {
        split("", out)
    }
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

        printf "%s:%d:%d: Unrecognized character\n", INPUT_FILE, line, col > "/dev/stderr"
        close(INPUT_FILE)
        close("/dev/stderr")
        exit 1
    }
    s["tokens", token_index, "type"] = "eof"
    s["tokens", token_index, "value"] = ""
    s["tokens", token_index, "line"] = line
    s["tokens", token_index, "col"] = col
    s["num_tokens"] = token_index
}

function parse(s,      a) {
    s["parser_state", "token_index"] = 1
    s["parser_state", "ast_index"] = 1

    a= ast_start_node(s, "program")
    parse_function_decl(s)
    ast_end_node(s, a)
    return a
}

function parse_function_decl(s,     a) {
    a = ast_start_node(s, "function_decl")
    parser_expect(s, "int")
    ast_set_node_attr(s, a, "name", parser_expect(s, "id"))
    parser_expect(s, "(")
    parser_expect(s, ")")
    parser_expect(s, "{")
    ast_set_node_attr(s, a, "body", parse_statment(s))
    parser_expect(s, "}")
    ast_end_node(s, a)
    return a
}

function parse_statment(s,     a) {
    a = ast_start_node(s, "return")
    parser_expect(s, "return")
    ast_set_node_attr(s, a, "expr", parse_expr(s))
    parser_expect(s, ";")
    ast_end_node(s, a)
    return a
}

function parse_expr(s,     a) {
    a = ast_start_node(s, "int")
    ast_set_node_attr(s, a, "value", parser_expect(s, "int"))
    ast_end_node(s, a)
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
    printf("%s:%d:%d: %s\n", s["input_file"], s["tokens", s["parser_state", "token_index"], "line"], s["tokens", s["parser_state", "token_index"], "col"], error_message) > "/dev/stderr"
    close("/dev/stderr")
    exit 1
}

function ast_start_node(s, node_type,       ast_index) {
    s["ast", s["parser_state", "ast_index"], "type"] = node_type
    s["ast", s["parser_state", "ast_index"], "line"] = s["tokens", s["parser_state", "token_index"], "line"]
    s["ast", s["parser_state", "ast_index"], "col"] = s["tokens", s["parser_state", "token_index"], "col"]
    ast_index = s["parser_state", "ast_index"]
    s["parser_state", "ast_index"]++
    return ast_index
}

function ast_end_node(s, ast_index) {
    s["ast", ast_index, "end"] = s["parser_state", "ast_index"] - 1
}

function ast_get_node_end(s, ast_index) {
    return s["ast", ast_index, "end"]
}

function ast_set_node_attr(s, ast_index, key, value) {
    s["ast", ast_index, key] = value
    list_append(s, "ast@" ast_index "@attrs", key)
}

function ast_get_node_attr(s, ast_index, key) {
    return s["ast", ast_index, key]
}

function ast_print(s) {
    print_ast_inner(s, 1, 0)
}

function print_ast_inner(s, ast_index, indent,    curr, end, i, attrs) {
    for (i = 0; i < indent; i++) {
        printf " "
    }
    printf "%s", s["ast", ast_index, "type"]
    list_get(s, "ast@" ast_index "@attrs", attrs)
    for (i in attrs) {
        printf " %s=%s", attrs[i], ast_get_node_attr(s, ast_index, attrs[i])
    }
    printf "\n"
    end = ast_get_node_end(s, ast_index)
    curr = ast_index + 1
    while (curr <= end) {
        curr = print_ast_inner(s, curr, indent + 2) + 1
    }
    return end
}

function lower(s) {
    ir_init(s)
    lower_function(s, 2)
}

function lower_function(s, ast_index) {
    ir_function(s, ast_get_node_attr(s, ast_index, "name"))
    ir_block(s)
    lower_statement(s, ast_index+1)
}

function lower_statement(s, ast_index) {
    ir_instruction_return(s, ast_get_node_attr(s, ast_get_node_attr(s, ast_index, "expr"), "value"))
}

function ir_init(s) {
    s["ir", "num_functions"] = 0
}

function ir_curr_function(s) {
    return "ir@functions@" s["ir", "num_functions"]
}

function ir_curr_block(s) {
    return ir_curr_function(s) "@blocks@" s[ir_curr_function(s), "num_blocks"]
}

function ir_curr_instruction(s) {
    return ir_curr_block(s) "@instructions@" s[ir_curr_block(s), "num_instructions"]
}

function ir_function(s, name) {
    s["ir", "num_functions"]++
    s[ir_curr_function(s), "name"] = name
    s[ir_curr_function(s), "num_blocks"] = 0
}

function ir_block(s) {
    s[ir_curr_function(s), "num_blocks"]++
    s[ir_curr_block(s), "num_instructions"] = 0
}

function ir_instruction_return(s, value) {
    s[ir_curr_block(s), "num_instructions"]++
    s[ir_curr_instruction(s), "type"] = "return"
    s[ir_curr_instruction(s), "value"] = value
}

function ir_print(s,    i, j, k) {
    for (i = 1; i <= s["ir", "num_functions"]; i++) {
        printf("function %s\n", s["ir", "functions", i, "name"])
        for (j = 1; j <= s["ir", "functions", i, "num_blocks"]; j++) {
            printf("  block %d\n", j)
            for (k = 1; k <= s["ir", "functions", i, "blocks", j, "num_instructions"]; k++) {
                printf("    ")
                ir_print_instruction(s, i, j, k)
            }
        }
    }
}

function ir_print_instruction(s, i, j, k) {
    printf("return %s\n", s["ir", "functions", i, "blocks", j, "instructions", k, "value"])
}

function codegen(s,     i, j, k) {
    for (i = 1; i <= s["ir", "num_functions"]; i++) {
        printf(".globl %s\n", s["ir", "functions", i, "name"])
        printf("%s:\n", s["ir", "functions", i, "name"])
        for (j = 1; j <= s["ir", "functions", i, "num_blocks"]; j++) {
            printf("%s.%d:\n", s["ir", "functions", i, "name"], j)
            for (k = 1; k <= s["ir", "functions", i, "blocks", j, "num_instructions"]; k++) {
                codegen_instruction(s, i, j, k)
            }
        }
    }
}

function codegen_instruction(s, i, j, k) {
    printf("li a0, %d\n", s["ir", "functions", i, "blocks", j, "instructions", k, "value"])
    printf("ret\n")
}
