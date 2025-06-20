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

    AST_ROOT = parse(S)

    print "/*"
    tree_print(S, AST_ROOT)
    print "*/\n"

    IR = lower(S, AST_ROOT)

    print "/*"
    tree_print(S, IR)
    print "*/\n"

    codegen(S, IR)
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

# function list_length(s, key,    temp, count) {
#     temp = s[key]
#     count = gsub(/:/, "", temp)
#     return count
# }
function list_contains(s, key, needle,    items, i) {
    list_get(s, key, items)

    for (i in items) {
        if (items[i] == needle) {
            return 1
        }
    }

    return 0
}

function tree_create_node(s, type) {
    if (!("tree" "@" "node_count" in s)) {
        s["tree", "node_count"] = 0
    }

    s["tree", "node_count"]++
    s["tree", s["tree", "node_count"], "type"] = type
    return s["tree", "node_count"]
}

function tree_get_type(s, node) {
    return s["tree", node, "type"]
}

function tree_assert_type(s, node, type) {
    if (tree_get_type(s, node) != type) {
        fatal(\
            sprintf(\
                "node %d, has type '%s', expected '%s'",
                node,
                tree_get_type(s, node),
                type\
            )\
        )
    }
}

function tree_has_attr(s, node, attr) {
    return list_contains(s, "@" node "@" "attrs", attr)
}

function tree_set_attr(s, node, attr, value) {
    s["tree", node, "attrs", attr] = value

    if (!tree_has_attr(s, node, attr)) {
        list_append(s, "@" node "@" "attrs", attr)
    }
}

function tree_get_attr(s, node, attr) {
    if (!tree_has_attr(s, node, attr)) {
        if (tree_has_child(s, node, attr)) {
            fatal(\
                sprintf(\
                    "node %d is missing attr '%s', but it has a child with the same name",
                    node,
                    attr\
                )\
            )
        }

        if ("tree" "@" node "@" "attrs" in s) {
            fatal(\
                sprintf(\
                    "node %d is missing attr '%s' (has '%s')",
                    node,
                    attr,
                    s["tree", node, "attrs"]\
                )\
            )
        }
        else {
            fatal(\
                sprintf(\
                    "node %d is missing attr '%s' (has no attrs)", node, attr\
                )\
            )
        }
    }

    return s["tree", node, "attrs", attr]
}

function tree_list_attrs(s, node, out) {
    list_get(s, "@" node "@" "attrs", out)
}

function tree_has_child(s, node, child_name) {
    return list_contains(s, "@" node "@" "children", child_name)
}

function tree_set_child(s, node, child_name, value) {
    s["tree", node, "children", child_name] = value

    if (!tree_has_child(s, node, child_name)) {
        list_append(s, "@" node "@" "children", child_name)
    }
}

function tree_get_child(s, node, child_name) {
    if (!tree_has_child(s, node, child_name)) {
        if (tree_has_attr(s, node, child_name)) {
            fatal(\
                sprintf(\
                    "node %d is missing child '%s', but it has an attr with the same name",
                    node,
                    child_name\
                )\
            )
        }

        if ("tree" "@" node "@" "children" in s) {
            fatal(\
                sprintf(\
                    "node %d is missing child '%s' (has '%s')",
                    node,
                    child_name,
                    s["tree", node, "children"]\
                )\
            )
        }
        else {
            fatal(\
                sprintf(\
                    "node %d is missing child '%s' (has no children)",
                    node,
                    child_name\
                )\
            )
        }
    }

    return s["tree", node, "children", child_name]
}

function tree_list_children(s, node, out) {
    list_get(s, "@" node "@" "children", out)
}

function tree_add_item(s, node, value) {
    if (!("tree" "@" node "@" "count_items" in s)) {
        s["tree", node, "count_items"] = 1
    }
    else {
        s["tree", node, "count_items"]++
    }

    s["tree", node, "items", s["tree", node, "count_items"]] = value
}

function tree_get_item(s, node, i) {
    if (i > tree_count_items(s, node)) {
        fatal(\
            sprintf(\
                "node %d is missing item '%d' (has %d items)",
                node,
                i,
                tree_count_items(s, node)\
            )\
        )
    }

    return s["tree", node, "items", i]
}

function tree_count_items(s, node) {
    if (!("tree" "@" node "@" "count_items" in s)) {
        return 0
    }

    return s["tree", node, "count_items"]
}

function tree_print(s, node) {
    tree_print_inner(s, node, 0)
}

function print_indent(indent,    i) {
    for (i = 0; i < indent; i++) {
        printf(" ")
    }
}

function tree_print_inner(s, node, indent,    i, j, attrs, children, count_items) {
    print_indent(indent)
    printf("%s(%d)", tree_get_type(s, node), node)

    tree_list_attrs(s, node, attrs)

    for (i in attrs) {
        printf(" %s=%s", attrs[i], tree_get_attr(s, node, attrs[i]))
    }

    printf("\n")

    tree_list_children(s, node, children)

    for (i in children) {
        print_indent(indent)
        printf("%s:\n", children[i])
        tree_print_inner(s, tree_get_child(s, node, children[i]), indent + 2)
    }

    count_items = tree_count_items(s, node)

    for (i = 1; i <= count_items; i++) {
        print_indent(indent)
        printf("%d:\n", i)
        tree_print_inner(s, tree_get_item(s, node, i), indent + 2)
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

function ast_create_node(s, type,    a) {
    a = tree_create_node(s, type)
    tree_set_attr(\
        s, a, "line", s["tokens", s["parser_state", "token_index"], "line"]\
    )
    tree_set_attr(\
        s, a, "col", s["tokens", s["parser_state", "token_index"], "col"]\
    )
    return a
}

function parse(s,    a) {
    s["parser_state", "token_index"] = 1

    a = ast_create_node(s, "program")
    tree_set_child(s, a, "function_decl", parse_function_decl(s))
    return a
}

function parse_function_decl(s,    a) {
    a = ast_create_node(s, "function_decl")
    parser_expect(s, "int")
    tree_set_attr(s, a, "name", parser_expect(s, "id"))
    parser_expect(s, "(")
    parser_expect(s, ")")
    parser_expect(s, "{")
    tree_set_child(s, a, "body", parse_statments(s))
    parser_expect(s, "}")
    return a
}

function parse_statments(s,    a) {
    a = ast_create_node(s, "statements")

    while (parser_peek(s) != "}" && parser_peek(s) != "eof") {
        tree_add_item(s, a, parse_statment(s))
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
    tree_set_child(s, a, "expr", parse_expr(s, 0))
    parser_expect(s, ";")
    return a
}

function parse_variable_declaration(s,    a) {
    a = ast_create_node(s, "variable_declaration")
    parser_expect(s, "int")
    tree_set_attr(s, a, "name", parser_expect(s, "id"))
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
        tree_set_attr(s, a, "value", parser_advance(s))
        return a
    }

    if (parser_accept(s, "-")) {
        a = ast_create_node(s, "negate")
        tree_set_child(s, a, "expr", parse_expr(s, 0))
        return a
    }

    if (parser_accept(s, "!")) {
        a = ast_create_node(s, "not")
        tree_set_child(s, a, "expr", parse_expr(s, 0))
        return a
    }

    if (parser_accept(s, "~")) {
        a = ast_create_node(s, "bitwise_not")
        tree_set_child(s, a, "expr", parse_expr(s, 0))
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

function ir_instruction_void(s, type, block,    l) {
    l = tree_create_node(s, type)
    tree_add_item(s, block, l)
    return l
}

function ir_instruction(s, type, block,    l) {
    l = tree_create_node(s, type)
    tree_add_item(s, block, l)

    if (!("lower_state@num_temporaries" in s)) {
        s["lower_state", "num_temporaries"] = 1
    }
    else {
        s["lower_state", "num_temporaries"]++
    }

    tree_set_attr(s, l, "variable", "temp$" s["lower_state", "num_temporaries"])

    return l
}

function ir_get_variable(s, l) {
    if (!tree_has_attr(s, l, "variable")) {
        fatal(\
            sprintf(\
                "ir_get_variable called on void instruction %s",
                tree_get_type(s, l)\
            )\
        )
    }

    return tree_get_attr(s, l, "variable")
}

function lower(s, ast_root) {
    tree_assert_type(s, ast_root, "program")
    return lower_function(s, tree_get_child(s, ast_root, "function_decl"))
}

function lower_function(s, a,    l, block) {
    tree_assert_type(s, a, "function_decl")
    l = tree_create_node(s, "function")
    tree_set_attr(s, l, "name", tree_get_attr(s, a, "name"))
    block = tree_create_node(s, "block")
    tree_add_item(s, l, block)
    tree_set_attr(s, block, "number", tree_count_items(s, l))
    lower_statements(s, tree_get_child(s, a, "body"), block)
    return l
}

function lower_statements(s, a, block,    i) {
    tree_assert_type(s, a, "statements")

    for (i = 1; i <= tree_count_items(s, a); i++) {
        lower_statement(s, tree_get_item(s, a, i), block)
    }
}

function lower_statement(s, a, block,    x, l) {
    tree_assert_type(s, a, "return")
    x = lower_expr(s, tree_get_child(s, a, "expr"), block)
    l = ir_instruction_void(s, "return", block)
    tree_set_attr(s, l, "x", ir_get_variable(s, x))
    return l
}

function lower_expr(s, a, block,    l, x) {
    if (tree_get_type(s, a) == "int") {
        l = ir_instruction(s, "constant", block)
        tree_set_attr(s, l, "value", tree_get_attr(s, a, "value"))
        return l
    }

    if (tree_get_type(s, a) == "bitwise_not") {
        x = lower_expr(s, tree_get_child(s, a, "expr"), block)
        l = ir_instruction(s, "bitwise_not", block)
        tree_set_attr(s, l, "x", ir_get_variable(s, x))
        return l
    }

    if (tree_get_type(s, a) == "negate") {
        x = lower_expr(s, tree_get_child(s, a, "expr"), block)
        l = ir_instruction(s, "negate", block)
        tree_set_attr(s, l, "x", ir_get_variable(s, x))
        return l
    }

    if (tree_get_type(s, a) == "not") {
        x = lower_expr(s, tree_get_child(s, a, "expr"), block)
        l = ir_instruction(s, "logical_not", block)
        tree_set_attr(s, l, "x", ir_get_variable(s, x))
        return l
    }

    fatal(\
        sprintf(\
            "node %d has type '%s', expected an expression",
            a,
            tree_get_type(s, a)\
        )\
    )
}

function codegen(s, ir_root) {
    codegen_function(s, ir_root)
}

function codegen_function(s, l,    funcname, i, block, j) {
    funcname = tree_get_attr(s, l, "name")
    printf(".globl %s\n", funcname)
    printf("%s:\n", funcname)

    for (i = 1; i <= tree_count_items(s, l); i++) {
        printf("%s.%d:\n", funcname, i)
        block = tree_get_item(s, l, i)

        for (j = 1; j <= tree_count_items(s, block); j++) {
            codegen_instruction(s, tree_get_item(s, block, j))
        }
    }
}

function codegen_get_register(s, variable) {
    return "a0"
}

function codegen_instruction(s, l,    type) {
    type = tree_get_type(s, l)

    if (type == "return") {
        if (codegen_get_register(s, tree_get_attr(s, l, "x")) != "a0") {
            printf(\
                "add a0, %s, x0\n",
                codegen_get_register(s, tree_get_attr(s, l, "x"))\
            )
        }

        printf("ret\n")
    }
    else if (type == "constant") {
        printf(\
            "li %s, %d\n",
            codegen_get_register(s, ir_get_variable(s, l)),
            tree_get_attr(s, l, "value")\
        )
    }
    else if (type == "negate") {
        printf(\
            "neg %s, %s\n",
            codegen_get_register(s, ir_get_variable(s, l)),
            codegen_get_register(s, tree_get_attr(s, l, "x"))\
        )
    }
    else if (type == "logical_not") {
        printf(\
            "seqz %s, %s\n",
            codegen_get_register(s, ir_get_variable(s, l)),
            codegen_get_register(s, tree_get_attr(s, l, "x"))\
        )
    }
    else if (type == "bitwise_not") {
        printf(\
            "not %s, %s\n",
            codegen_get_register(s, ir_get_variable(s, l)),
            codegen_get_register(s, tree_get_attr(s, l, "x"))\
        )
    }
    else {
        fatal(sprintf("can't codegen unknown instruction '%s'", type))
    }
}
