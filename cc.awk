#!/usr/bin/env -S awk --posix --lint --exec
BEGIN {
    SUBSEP = "@"

    if (ARGC != 2) {
        print "Usage: cc.awk program.c"
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
    # print "/*"
    # for (I = 1; I <= S["num_tokens"]; I++) {
    #     print S["tokens", I, "type"], S["tokens", I, "value"]
    # }
    # printf("*/\n")
    close(INPUT_FILE)

    parse(S)

    print "/*"
    tree_print(S, "ast", 1)
    print "*/\n"

    lower(S)

    print "/*"
    ir_print(S)
    print "*/\n"

    codegen(S)
}

function fatal(string) {
    print string >> "/dev/stderr"
    close("/dev/stderr")
    exit 1
}

function list_append(s, key, value) {
    if (key in s && s[key]) {
        s[key] = s[key] ":" value
    }
    else {
        s[key] = value
    }
}

function list_get(s, key, out,    i) {
    if (key in s) {
        split(s[key], out, ":")
    }
    else {
        split("", out)
    }
}

function list_length(s, key,    temp, count) {
    temp = s[key]
    count = gsub(/:/, "", temp)
    return count
}

function list_contains(s, key, needle,    items, i) {
    list_get(s, key, items)

    for (i in items) {
        if (items[i] == needle) {
            return 1
        }
    }

    return 0
}

function tree_create_node(s, tree_name, type) {
    if (!(tree_name "@" "node_count" in s)) {
        s[tree_name, "node_count"] = 0
    }

    s[tree_name, "node_count"]++
    s[tree_name, s[tree_name, "node_count"], "type"] = type
    return s[tree_name, "node_count"]
}

function tree_get_type(s, tree_name, node) {
    return s[tree_name, node, "type"]
}

function tree_has_attr(s, tree_name, node, attr) {
    return list_contains(s, tree_name "@" node "@" "attrs", attr)
}

function tree_set_attr(s, tree_name, node, attr, value) {
    s[tree_name, node, "attrs", attr] = value

    if (!tree_has_attr(s, tree_name, node, attr)) {
        list_append(s, tree_name "@" node "@" "attrs", attr)
    }
}

function tree_get_attr(s, tree_name, node, attr) {
    if (!tree_has_attr(s, tree_name, node, attr)) {
        fatal(\
            sprintf(\
                "Node %d is missing attr %s (has '%s')",
                node,
                attr,
                s[tree_name, node, "attrs"]\
            )\
        )
    }

    return s[tree_name, node, "attrs", attr]
}

function tree_list_attrs(s, tree_name, node, out) {
    list_get(s, tree_name "@" node "@" "attrs", out)
}

function tree_has_child(s, tree_name, node, child_name) {
    return list_contains(s, tree_name "@" node "@" "children", child_name)
}

function tree_set_child(s, tree_name, node, child_name, value) {
    s[tree_name, node, "children", child_name] = value

    if (!tree_has_child(s, tree_name, node, child_name)) {
        list_append(s, tree_name "@" node "@" "children", child_name)
    }
}

function tree_get_child(s, tree_name, node, child_name) {
    if (!tree_has_child(s, tree_name, node, child_name)) {
        fatal(\
            sprintf(\
                "Node %d is missing child %s (has '%s')",
                node,
                child_name,
                s[tree_name, node, "children"]\
            )\
        )
    }

    return s[tree_name, node, "children", child_name]
}

function tree_list_children(s, tree_name, node, out) {
    list_get(s, tree_name "@" node "@" "children", out)
}

function tree_add_item(s, tree_name, node, value) {
    if (!(tree_name "@" node "@" "count_items" in s)) {
        s[tree_name, node, "count_items"] = 1
    }
    else {
        s[tree_name, node, "count_items"]++
    }

    s[tree_name, node, "items", s[tree_name, node, "count_items"]] = value
}

function tree_get_item(s, tree_name, node, i) {
    if (i > tree_count_items(s, tree_name, node)) {
        fatal(\
            sprintf(\
                "Node %d is missing item %d (has '%d')",
                node,
                i,
                tree_count_items(s, tree_name, node)\
            )\
        )
    }

    return s[tree_name, node, "items", i]
}

function tree_count_items(s, tree_name, node) {
    if (!(tree_name "@" node "@" "count_items" in s)) {
        return 0
    }

    return s[tree_name, node, "count_items"]
}

function tree_print(s, tree_name, node) {
    tree_print_inner(s, tree_name, node, 0)
}

function print_indent(indent,    i) {
    for (i = 0; i < indent; i++) {
        printf(" ")
    }
}

function tree_print_inner(s, tree_name, node, indent,    i, j, attrs, children, count_items) {
    print_indent(indent)
    printf("type: %s\n", tree_get_type(s, tree_name, node))

    tree_list_attrs(s, tree_name, node, attrs)

    for (i in attrs) {
        print_indent(indent)
        printf(\
            "%s: %s\n", attrs[i], tree_get_attr(s, tree_name, node, attrs[i])\
        )
    }

    tree_list_children(s, tree_name, node, children)

    for (i in children) {
        print_indent(indent)
        printf("%s:\n", children[i])
        tree_print_inner(\
            s,
            tree_name,
            tree_get_child(s, tree_name, node, children[i]),
            indent + 2\
        )
    }

    count_items = tree_count_items(s, tree_name, node)

    for (i = 1; i <= count_items; i++) {
        print_indent(indent)
        printf("%d:\n", i)
        tree_print_inner(\
            s, tree_name, tree_get_item(s, tree_name, node, i), indent + 2\
        )
    }
}

function lex(s, source,    line, col, i, token_index, symbol, found, symbols) {
    token_index = 1
    split(\
        "auto break case char const continue default do double else enum extern float for goto if int long register return short signed sizeof static struct switch typedef union unsigned void volatile while >>= <<= += -= *= /= %= &= ^= |= >> << ++ -- -> && || <= >= == != ; | { } , = ( ) [ ] . & ! ~ - + * / % < > ^ | ?",
        symbols,
        " "\
    )
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
            }
            else {
                source = substr(source, 2)
            }

            continue
        }

        found = 0

        for (i in symbols) {
            symbol = symbols[i]

            if (\
                length(source) >= length(symbol) &&
                substr(source, 1, length(symbol)) == symbol\
            ) {
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

        match(source, /^[a-zA-Z][a-zA-Z0-9_]*/)

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

        close(s["input_file"])
        fatal(\
            sprintf(\
                "%s:%d:%d: Unrecognized character\n",
                s["input_file"],
                line,
                col > "/dev/stderr"\
            )\
        )
    }

    s["tokens", token_index, "type"] = "eof"
    s["tokens", token_index, "value"] = ""
    s["tokens", token_index, "line"] = line
    s["tokens", token_index, "col"] = col
    s["num_tokens"] = token_index
}

function ast_create_node(s, type) {
    return tree_create_node(s, "ast", type)
}

function ast_get_type(s, node) {
    return tree_get_type(s, "ast", node)
}

function ast_has_attr(s, node, attr) {
    return tree_has_attr(s, "ast", node, attr)
}

function ast_set_attr(s, node, attr, value) {
    return tree_set_attr(s, "ast", node, attr, value)
}

function ast_get_attr(s, node, attr) {
    return tree_get_attr(s, "ast", node, attr)
}

function ast_list_attrs(s, node, out) {
    return tree_list_attrs(s, "ast", node, out)
}

function ast_has_child(s, node, child_name) {
    return tree_has_child(s, "ast", node, child_name)
}

function ast_set_child(s, node, child_name, value) {
    return tree_set_child(s, "ast", node, child_name, value)
}

function ast_get_child(s, node, child_name) {
    return tree_get_child(s, "ast", node, child_name)
}

function ast_list_children(s, node, out) {
    return tree_list_children(s, "ast", node, out)
}

function ast_add_item(s, node, value) {
    return tree_add_item(s, "ast", node, value)
}

function ast_get_item(s, node, i) {
    return tree_get_item(s, "ast", node, i)
}

function ast_count_items(s, node) {
    return tree_count_items(s, "ast", node)
}

function ast_print(s, node) {
    return tree_print(s, "ast", node)
}

function parse(s,    a) {
    s["parser_state", "token_index"] = 1

    a = ast_create_node(s, "program")
    ast_set_child(s, a, "function_decl", parse_function_decl(s))
    return a
}

function parse_function_decl(s,    a) {
    a = ast_create_node(s, "function_decl")
    parser_expect(s, "int")
    ast_set_attr(s, a, "name", parser_expect(s, "id"))
    parser_expect(s, "(")
    parser_expect(s, ")")
    parser_expect(s, "{")
    ast_set_child(s, a, "body", parse_statments(s))
    parser_expect(s, "}")
    return a
}

function parse_statments(s,    a) {
    a = ast_create_node(s, "statements")

    while (parser_peek(s) != "}" && parser_peek(s) != "eof") {
        ast_add_item(s, a, parse_statment(s))
    }

    return a
}

function parse_statment(s,    a) {
    if (parser_peek(s) == "return") {
        return parse_return_statement(s)
    }

    if (parser_peek(s) == "int") {
        return parse_variable_declaration(s)
    }

    if (parser_peek(s) == "{") {
        parser_expect(s, "{")
        a = parse_statments(s)
        parser_expect(s, "}")
        return a
    }

    return parse_expr(s, 1)
}

function parse_return_statement(s,    a) {
    a = ast_create_node(s, "return")
    parser_expect(s, "return")
    ast_set_child(s, a, "expr", parse_expr(s, 0))
    parser_expect(s, ";")
    return a
}

function parse_variable_declaration(s,    a) {
    a = ast_create_node(s, "variable_declaration")
    parser_expect(s, "int")
    ast_set_attr(s, a, "name", parser_expect(s, "id"))
    parser_expect(s, ";")
    return a
}

function parse_expr(s, or_statement,    a) {
    a = parse_atom(s, or_statement)
    return a
}

function parse_atom(s, or_statement,    a) {
    if (parser_peek(s) == "int") {
        a = ast_create_node(s, "int")
        ast_set_attr(s, a, "value", parser_advance(s))
        return a
    }

    if (parser_accept(s, "-")) {
        a = ast_create_node(s, "negate")
        ast_set_attr(s, a, "expr", parse_expr(s, 0))
        return a
    }

    if (parser_accept(s, "!")) {
        a = ast_create_node(s, "not")
        ast_set_attr(s, a, "expr", parse_expr(s, 0))
        return a
    }

    if (parser_accept(s, "~")) {
        a = ast_create_node(s, "bitwise_not")
        ast_set_attr(s, a, "expr", parse_expr(s, 0))
        return a
    }

    if (parser_accept(s, "(")) {
        a = parse_expr(s, 0)
        parser_expect(s, ")")
        return a
    }

    if (or_statement) {
        parser_error(s, "Expected expression or statement")
    }
    else {
        parser_error(s, "Expected expression")
    }
}

function parser_expect(s, token_type,    value) {
    if (parser_peek(s) != token_type) {
        parser_error(\
            s, sprintf("Expected %s, found %s", token_type, parser_peek(s))\
        )
    }

    return parser_advance(s)
}

function parser_peek(s) {
    return s["tokens", s["parser_state", "token_index"], "type"]
}

function parser_accept(s, token_type) {
    if (parser_peek(s) == token_type) {
        parser_advance(s)
        return 1
    }

    return 0
}

function parser_advance(s,    value) {
    value = s["tokens", s["parser_state", "token_index"], "value"]
    s["parser_state", "token_index"]++
    return value
}

function parser_error(s, error_message) {
    fatal(\
        sprintf(\
            "%s:%d:%d: %s\n",
            s["input_file"],
            s["tokens", s["parser_state", "token_index"], "line"],
            s["tokens", s["parser_state", "token_index"], "col"],
            error_message\
        )\
    )
}

function lower(s) {
    ir_init(s)
    lower_function(s, 2)
}

function lower_function(s, a) {
    ir_function(s, ast_get_attr(s, a, "name"))
    ir_block(s)
    lower_statements(s, ast_get_attr(s, a, "body"))
}

function lower_statements(s, a,    i) {}

function lower_statement(s, a,    x, instr) {
    x = lower_expr(s, ast_get_attr(s, a, "expr"))
    instr = ir_instruction_void(s, "return")
    ir_instruction_set_attr(s, instr, "x", x)
}

function lower_expr(s, a,    instr, x) {
    if (ast_get_type(s, a) == "int") {
        instr = ir_instruction(s, "constant")
        ir_instruction_set_attr(s, instr, "c", ast_get_attr(s, a, "value"))
        return ir_instruction_get_variable(s, instr)
    }

    if (ast_get_type(s, a) == "bitwise_not") {
        x = lower_expr(s, ast_get_attr(s, a, "expr"))
        instr = ir_instruction(s, "bitwise_not")
        ir_instruction_set_attr(s, instr, "x", x)
        return ir_instruction_get_variable(s, instr)
    }

    if (ast_get_type(s, a) == "negate") {
        x = lower_expr(s, ast_get_attr(s, a, "expr"))
        instr = ir_instruction(s, "negate")
        ir_instruction_set_attr(s, instr, "x", x)
        return ir_instruction_get_variable(s, instr)
    }

    if (ast_get_type(s, a) == "not") {
        x = lower_expr(s, ast_get_attr(s, a, "expr"))
        instr = ir_instruction(s, "logical_not")
        ir_instruction_set_attr(s, instr, "x", x)
        return ir_instruction_get_variable(s, instr)
    }
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
    return ir_curr_block(s) "@instructions@"\
        s[ir_curr_block(s), "num_instructions"]
}

function ir_function(s, name) {
    s["ir", "num_functions"]++
    s[ir_curr_function(s), "name"] = name
    s[ir_curr_function(s), "num_blocks"] = 0
    s[ir_curr_function(s), "num_variables"] = 0
}

function ir_block(s) {
    s[ir_curr_function(s), "num_blocks"]++
    s[ir_curr_block(s), "num_instructions"] = 0
}

function ir_instruction_void(s, type) {
    s[ir_curr_block(s), "num_instructions"]++
    s[ir_curr_instruction(s), "type"] = type
    s[ir_curr_instruction(s), "variable"] = ""
    return ir_curr_instruction(s)
}

function ir_instruction(s, type) {
    s[ir_curr_block(s), "num_instructions"]++
    s[ir_curr_instruction(s), "type"] = type
    s[ir_curr_function(s), "num_variables"]++
    s[ir_curr_instruction(s), "variable"] = "$"\
        s[ir_curr_function(s), "num_variables"]
    return ir_curr_instruction(s)
}

function ir_instruction_set_attr(s, instr, key, value) {
    s[instr, key] = value
    list_append(s, instr "@attrs", key)
}

function ir_instruction_get_variable(s, instr, key, value) {
    return s[ir_curr_instruction(s), "variable"]
}

function ir_print(s,    i, j, k) {
    for (i = 1; i <= s["ir", "num_functions"]; i++) {
        printf("%s() {\n", s["ir", "functions", i, "name"])

        for (j = 1; j <= s["ir", "functions", i, "num_blocks"]; j++) {
            printf("  %s.%d:\n", s["ir", "functions", i, "name"], j)

            for (k = 1; k <= s[\
                "ir", "functions", i, "blocks", j, "num_instructions"\
            ]; k++) {
                printf("    ")
                ir_print_instruction(s, i, j, k)
            }
        }

        printf("}\n")
    }
}

function ir_print_instruction(s, i, j, k,    instr, attrs) {
    instr = "ir@functions@" i "@blocks@" j "@instructions@" k

    if (s[instr, "variable"]) {
        printf("%s <- ", s[instr, "variable"])
    }

    printf("%s", s[instr, "type"])
    list_get(s, instr "@attrs", attrs)

    for (i in attrs) {
        printf(" %s=%s", attrs[i], s[instr, attrs[i]])
    }

    printf("\n")
}

function codegen(s,    i) {
    for (i = 1; i <= s["ir", "num_functions"]; i++) {
        codegen_function(s, i)
    }
}

function codegen_function(s, i,    j, k) {
    printf(".globl %s\n", s["ir", "functions", i, "name"])
    printf("%s:\n", s["ir", "functions", i, "name"])

    for (j = 1; j <= s["ir", "functions", i, "num_blocks"]; j++) {
        printf("%s.%d:\n", s["ir", "functions", i, "name"], j)

        for (k = 1; k <= s[\
            "ir", "functions", i, "blocks", j, "num_instructions"\
        ]; k++) {
            codegen_instruction(\
                s, "ir@functions@" i "@blocks@" j "@instructions@" k\
            )
        }
    }
}

function codegen_get_register(s, variable) {
    return "a0"
}

function codegen_instruction(s, instr) {
    if (s[instr, "type"] == "return") {
        if (codegen_get_register(s, s[instr, "x"]) != "a0") {
            printf("add a0, %s, x0\n", codegen_get_register(s, s[instr, "x"]))
        }

        printf("ret\n")
    }
    else if (s[instr, "type"] == "constant") {
        printf(\
            "li %s, %d\n",
            codegen_get_register(s, s[instr, "variable"]),
            s[instr, "c"]\
        )
    }
    else if (s[instr, "type"] == "negate") {
        printf(\
            "neg %s, %s\n",
            codegen_get_register(s, s[instr, "variable"]),
            codegen_get_register(s, s[instr, "x"])\
        )
    }
    else if (s[instr, "type"] == "logical_not") {
        printf(\
            "seqz %s, %s\n",
            codegen_get_register(s, s[instr, "variable"]),
            codegen_get_register(s, s[instr, "x"])\
        )
    }
    else if (s[instr, "type"] == "bitwise_not") {
        printf(\
            "not %s, %s\n",
            codegen_get_register(s, s[instr, "variable"]),
            codegen_get_register(s, s[instr, "x"])\
        )
    }
    else {
        parser_error(s, sprintf("UNKNOWN INSTRUCTION %s", s[instr, "type"]))
    }
}
