#!/usr/bin/env -S awk -f
BEGIN {
    SUBSEP = "@"

    for (ARGNO = 1; ARGNO < ARGC; ARGNO++) {
        ARGV_COPY[ARGNO] = ARGV[ARGNO]
        delete ARGV[ARGNO]
    }

    main(ARGV_COPY, ARGC)
}

function main(argv, argc,    i, arg, infile, found_infile, outfile, found_outfile, arch, found_arch, source, line, s, ast_root, ir) {
    i = 1
    found_outfile = 0
    found_infile = 0
    found_arch = 0

    while (i < argc) {
        if (argv[i] == "-o") {
            found_outfile = 1
            outfile = argv[++i]
        }
        else if (argv[i] == "-a") {
            found_arch = 1
            arch = argv[++i]

            if (arch != "riscv") {
                print_help()
                exit 1
            }
        }
        else if (argv[i] == "-h") {
            print_help()
            return
        }
        else if (substr(argv[i], 1, 1) == "-") {
            print_help()
            exit 1
        }
        else {
            found_infile = 1
            infile = argv[i]
        }

        i++
    }

    if (!found_infile) {
        print_help()
        exit 1
    }

    if (!found_arch) {
        print_help()
        exit 1
    }

    if (!found_outfile) {
        outfile = infile
        sub(/\.c$/, ".s", outfile)
    }

    source = ""

    while ((getline line < infile) > 0) {
        source = source line "\n"
    }

    s["input_file"] = infile
    s["output_file"] = outfile

    lex(s, source)
    close(infile)

    ast_root = parse(s)

    print "/*" > outfile
    tree_print(s, ast_root)
    print "*/\n" > outfile

    ir = lower(s, ast_root)

    print "/*" > outfile
    tree_print(s, ir)
    print "*/\n" > outfile

    codegen(s, ir)

    close(outfile)
}

function print_help() {}

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
        fatal(sprintf("node %d has type '%s', expected '%s'", node, tree_get_type(s, node), type))
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
            fatal(sprintf("node %d is missing attr '%s', but it has a child with the same name", node, attr))
        }

        if ("tree" "@" node "@" "attrs" in s) {
            fatal(sprintf("node %d is missing attr '%s' (has '%s')", node, attr, s["tree", node, "attrs"]))
        }
        else {
            fatal(sprintf("node %d is missing attr '%s' (has no attrs)", node, attr))
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
            fatal(sprintf("node %d is missing child '%s', but it has an attr with the same name", node, child_name))
        }

        if ("tree" "@" node "@" "children" in s) {
            fatal(sprintf("node %d is missing child '%s' (has '%s')", node, child_name, s["tree", node, "children"]))
        }
        else {
            fatal(sprintf("node %d is missing child '%s' (has no children)", node, child_name))
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
        fatal(sprintf("node %d is missing item '%d' (has %d items)", node, i, tree_count_items(s, node)))
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
    tree_print_inner(s, node, 0, s["output_file"])
}

function print_indent(out, indent,    i) {
    for (i = 0; i < indent; i++) {
        printf(" ") > out
    }
}

function tree_print_inner(s, node, indent, out,    i, j, attrs, children, count_items) {
    print_indent(out, indent)
    printf("%s(%d)", tree_get_type(s, node), node) > out

    tree_list_attrs(s, node, attrs)

    for (i in attrs) {
        printf(" %s=%s", attrs[i], tree_get_attr(s, node, attrs[i])) > out
    }

    printf("\n") > out

    tree_list_children(s, node, children)

    for (i in children) {
        print_indent(out, indent)
        printf("%s:\n", children[i]) > out
        tree_print_inner(s, tree_get_child(s, node, children[i]), indent + 2, out)
    }

    count_items = tree_count_items(s, node)

    for (i = 1; i <= count_items; i++) {
        print_indent(out, indent)
        printf("%d:\n", i) > out
        tree_print_inner(s, tree_get_item(s, node, i), indent + 2, out)
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
        fatal(sprintf("%s:%d:%d: Unrecognized character\n", s["input_file"], line, col > "/dev/stderr"))
    }

    s["tokens", token_index, "type"] = "eof"
    s["tokens", token_index, "value"] = ""
    s["tokens", token_index, "line"] = line
    s["tokens", token_index, "col"] = col
    s["num_tokens"] = token_index
}

function ast_create_node(s, type,    a) {
    a = tree_create_node(s, type)
    tree_set_attr(s, a, "line", s["tokens", s["parser_state", "token_index"], "line"])
    tree_set_attr(s, a, "col", s["tokens", s["parser_state", "token_index"], "col"])
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
    tree_set_child(s, a, "body", parse_statements(s))
    parser_expect(s, "}")
    return a
}

function parse_statements(s,    a) {
    a = ast_create_node(s, "statements")

    while (parser_peek(s) != "}" && parser_peek(s) != "eof") {
        tree_add_item(s, a, parse_statement(s))
    }

    return a
}

function parse_statement(s,    a, expr) {
    if (parser_peek(s) == "return") {
        return parse_return_statement(s)
    }

    if (parser_peek(s) == "int") {
        return parse_variable_declaration(s)
    }

    if (parser_peek(s) == "if") {
        return parse_if_statement(s)
    }

    if (parser_peek(s) == "{") {
        parser_expect(s, "{")
        a = parse_statements(s)
        parser_expect(s, "}")
        return a
    }

    expr = parse_expr(s, 1)
    parser_expect(s, ";")
    a = ast_create_node(s, "expr_stat")
    tree_set_child(s, a, "expr", expr)
    return a
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

function parse_if_statement(s,    a) {
    a = ast_create_node(s, "if")
    parser_expect(s, "if")
    parser_expect(s, "(")
    tree_set_child(s, a, "condition", parse_expr(s, 0))
    parser_expect(s, ")")
    tree_set_child(s, a, "true_block", parse_statement(s))

    if (parser_accept(s, "else")) {
        tree_set_child(s, a, "false_block", parse_statement(s))
    }

    return a
}

function parse_expr(s, or_statement) {
    return parse_expr_with_precedence(s, or_statement, 0)
}

function parse_expr_with_precedence(s, or_statement, min_precedence,    a, op, lhs, rhs, precedence, associativity, op_name) {
    precedence["="] = 20
    associativity["="] = "right"
    op_name["="] = "assign"

    precedence["+"] = 120
    associativity["+"] = "left"
    op_name["+"] = "add"

    precedence["-"] = 120
    associativity["-"] = "left"
    op_name["-"] = "sub"

    precedence["*"] = 130
    associativity["*"] = "left"
    op_name["*"] = "mul"

    precedence["/"] = 130
    associativity["/"] = "left"
    op_name["/"] = "div"

    precedence["%"] = 130
    associativity["%"] = "left"
    op_name["%"] = "mod"

    a = parse_atom(s, or_statement)

    op = parser_peek(s)

    while (op in precedence && precedence[op] >= min_precedence) {
        parser_advance(s)

        if (associativity[op] == "right") {
            rhs = parse_expr_with_precedence(s, 0, precedence[op])
        }
        else {
            rhs = parse_expr_with_precedence(s, 0, precedence[op] + 1)
        }

        lhs = a
        a = tree_create_node(s, op_name[op])
        tree_set_attr(s, a, "line", tree_get_attr(s, lhs, "line"))
        tree_set_attr(s, a, "col", tree_get_attr(s, lhs, "col"))
        tree_set_child(s, a, "lhs", lhs)
        tree_set_child(s, a, "rhs", rhs)

        op = parser_peek(s)
    }

    return a
}

function parse_atom(s, or_statement,    a, name) {
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

    if (parser_peek(s) == "id") {
        name = parser_advance(s)
        a = ast_create_node(s, "variable")
        tree_set_attr(s, a, "name", name)
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
        parser_error(s, sprintf("Expected %s, found %s", token_type, parser_peek(s)))
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

function ir_instruction_void(s, type, cursor,    l) {
    l = tree_create_node(s, type)
    tree_add_item(s, cursor["block"], l)
    return l
}

function ir_instruction_temp(s, type, cursor,    l) {
    l = tree_create_node(s, type)
    tree_add_item(s, cursor["block"], l)

    if (!("lower_state@num_temporaries" in s)) {
        s["lower_state", "num_temporaries"] = 1
    }
    else {
        s["lower_state", "num_temporaries"]++
    }

    tree_set_attr(s, l, "variable", "temp$" s["lower_state", "num_temporaries"])

    return l
}

function ir_is_variable_temp(variable) {
    return length(variable) >= 5 && substr(variable, 1, 5) == "temp$"
}

function ir_get_variable(s, l) {
    if (!tree_has_attr(s, l, "variable")) {
        fatal(sprintf("ir_get_variable called on void instruction %s", tree_get_type(s, l)))
    }

    return tree_get_attr(s, l, "variable")
}

function ir_set_operand1(s, l, x) {
    tree_set_attr(s, l, "operand1", ir_get_variable(s, x))
}

function ir_get_operand1(s, l) {
    return tree_get_attr(s, l, "operand1")
}

function ir_set_operand2(s, l, x) {
    tree_set_attr(s, l, "operand2", ir_get_variable(s, x))
}

function ir_get_operand2(s, l) {
    return tree_get_attr(s, l, "operand2")
}

function ir_set_target(s, l, target) {
    tree_set_attr(s, l, "target", target)
}

function lower(s, ast_root) {
    tree_assert_type(s, ast_root, "program")
    return lower_function(s, tree_get_child(s, ast_root, "function_decl"))
}

function lower_function(s, a,    l, cursor) {
    tree_assert_type(s, a, "function_decl")
    l = tree_create_node(s, "function")
    tree_set_attr(s, l, "name", tree_get_attr(s, a, "name"))
    cursor["function"] = l
    cursor["block"] = tree_create_node(s, "block")
    tree_add_item(s, cursor["function"], cursor["block"])
    tree_set_attr(s, cursor["block"], "number", tree_count_items(s, l))
    lower_statement(s, tree_get_child(s, a, "body"), cursor)
    return l
}

function lower_statements(s, a, cursor,    i) {
    tree_assert_type(s, a, "statements")

    for (i = 1; i <= tree_count_items(s, a); i++) {
        lower_statement(s, tree_get_item(s, a, i), cursor)
    }
}

function lower_statement(s, a, cursor,    x, l, type) {
    type = tree_get_type(s, a)

    if (type == "variable_declaration") {
        # do nothing
    }
    else if (type == "return") {
        x = lower_expr(s, tree_get_child(s, a, "expr"), cursor)
        l = ir_instruction_void(s, "return", cursor)
        ir_set_operand1(s, l, x)
    }
    else if (type == "if") {
        lower_if(s, a, cursor)
    }
    else if (type == "expr_stat") {
        lower_expr(s, tree_get_child(s, a, "expr"), cursor)
    }
    else if (type == "statements") {
        lower_statements(s, a, cursor)
    }
    else {
        fatal(sprintf("node %d has type '%s', expected a statement", a, tree_get_type(s, a)))
    }
}

function lower_if(s, a, cursor,    has_false, cond, jz, jmp, false_block, after_block) {
    tree_assert_type(s, a, "if")
    has_false = tree_has_child(s, a, "false_block")
    #
    # jz condition after_block
    #  ... (if true)
    # after_block:
    # ...
    # Or
    # jz condition false_block
    #  ... (if true)
    #  jmp after_block
    # false_block:
    #  ... (if false)
    # after_block:
    # ...
    if (has_false) {
        false_block = lower_create_block(s, cursor)
        lower_statement_to_block(s, tree_get_child(s, a, "false_block"), cursor, false_block)
    }

    after_block = lower_create_block(s, cursor)

    cond = lower_expr(s, tree_get_child(s, a, "condition"), cursor)
    jz = ir_instruction_void(s, "jz", cursor)
    ir_set_operand1(s, jz, cond)

    if (has_false) {
        ir_set_target(s, jz, false_block)
    }
    else {
        ir_set_target(s, jz, after_block)
    }

    lower_statement(s, tree_get_child(s, a, "true_block"), cursor)

    if (has_false) {
        jmp = ir_instruction_void(s, "jmp", cursor)
        ir_set_target(s, jmp, after_block)
    }

    lower_switch_block(s, cursor, after_block)
}

function lower_statement_to_block(s, a, cursor, block,    oldblock) {
    oldblock = cursor["block"]
    lower_switch_block(s, cursor, block)
    lower_statement(s, a, cursor)
    lower_switch_block(s, cursor, oldblock)
}

function lower_create_block(s, cursor,    block) {
    block = tree_create_node(s, "block")
    tree_add_item(s, cursor["function"], block)
    return block
}

function lower_switch_block(s, cursor, block) {
    cursor["block"] = block
}

function lower_expr(s, a, cursor,    l, x, variable_name, lhs, rhs, binops, i, type) {
    type = tree_get_type(s, a)

    if (type == "int") {
        l = ir_instruction_temp(s, "constant", cursor)
        tree_set_attr(s, l, "value", tree_get_attr(s, a, "value"))
        return l
    }

    if (type == "bitwise_not") {
        x = lower_expr(s, tree_get_child(s, a, "expr"), cursor)
        l = ir_instruction_temp(s, "bitwise_not", cursor)
        ir_set_operand1(s, l, x)
        return l
    }

    if (type == "negate") {
        x = lower_expr(s, tree_get_child(s, a, "expr"), cursor)
        l = ir_instruction_temp(s, "negate", cursor)
        ir_set_operand1(s, l, x)
        return l
    }

    if (type == "not") {
        x = lower_expr(s, tree_get_child(s, a, "expr"), cursor)
        l = ir_instruction_temp(s, "logical_not", cursor)
        ir_set_operand1(s, l, x)
        return l
    }

    if (type == "assign") {
        lhs = tree_get_child(s, a, "lhs")
        rhs = tree_get_child(s, a, "rhs")
        tree_assert_type(s, lhs, "variable")
        variable_name = tree_get_attr(s, lhs, "name")
        x = lower_expr(s, rhs, cursor)

        if (ir_is_variable_temp(ir_get_variable(s, x))) {
            tree_set_attr(s, x, "variable", variable_name)
            return x
        }
        else {
            l = ir_instruction_temp(s, "mov", cursor)
            tree_set_attr(s, l, "variable", variable_name)
            ir_set_operand1(s, l, x)
            return l
        }
    }

    if (type == "variable") {
        # Return fake instruction that will not be added to the cursor that only contains the "variable" field
        l = tree_create_node(s, "variable")
        tree_set_attr(s, l, "variable", tree_get_attr(s, a, "name"))
        return l
    }

    split("add sub mul div mod", binops)

    for (i in binops) {
        if (type == binops[i]) {
            lhs = lower_expr(s, tree_get_child(s, a, "lhs"), cursor)
            rhs = lower_expr(s, tree_get_child(s, a, "rhs"), cursor)
            l = ir_instruction_temp(s, binops[i], cursor)
            ir_set_operand1(s, l, lhs)
            ir_set_operand2(s, l, rhs)
            return l
        }
    }

    fatal(sprintf("node %d has type '%s', expected an expression", a, type))
}

function find_usage(s, funct, out,    k, i, j, block, instr, variable, found_first_use, found_last_use) {
    k = 1
    out["first_use_count"] = 0
    out["last_use_count"] = 0

    for (i = 1; i <= tree_count_items(s, funct); i++) {
        block = tree_get_item(s, funct, i)

        for (j = 1; j <= tree_count_items(s, block); j++) {
            instr = tree_get_item(s, block, j)

            if (tree_has_attr(s, instr, "variable")) {
                variable = ir_get_variable(s, instr)

                if (!("first_uses_map" "@" variable in out)) {
                    out["first_use_count"]++
                    out["first_uses_map", variable] = k
                    out["first_uses_list", out["first_use_count"], "variable"] = variable
                    out["first_uses_list", out["first_use_count"], "location"] = k
                }
            }

            if (tree_has_attr(s, instr, "operand1")) {
                variable = ir_get_operand1(s, instr)

                if (!("first_uses_map" "@" variable in out)) {
                    out["first_use_count"]++
                    out["first_uses_map", variable] = k
                    out["first_uses_list", out["first_use_count"], "variable"] = variable
                    out["first_uses_list", out["first_use_count"], "location"] = k
                }
            }

            if (tree_has_attr(s, instr, "operand2")) {
                variable = ir_get_operand2(s, instr)

                if (!("first_uses_map" "@" variable in out)) {
                    out["first_use_count"]++
                    out["first_uses_map", variable] = k
                    out["first_uses_list", out["first_use_count"], "variable"] = variable
                    out["first_uses_list", out["first_use_count"], "location"] = k
                }
            }

            k++
        }
    }

    k--

    for (i = tree_count_items(s, funct); i >= 1; i--) {
        block = tree_get_item(s, funct, i)

        for (j = tree_count_items(s, block); j >= 1; j--) {
            instr = tree_get_item(s, block, j)

            if (tree_has_attr(s, instr, "variable")) {
                variable = ir_get_variable(s, instr)

                if (!("last_uses_map" "@" variable in out)) {
                    out["last_use_count"]++
                    out["last_uses_map", variable] = k
                    out["last_uses_list", out["last_use_count"], "variable"] = variable
                    out["last_uses_list", out["last_use_count"], "location"] = k
                }
            }

            if (tree_has_attr(s, instr, "operand1")) {
                variable = ir_get_operand1(s, instr)

                if (!("last_uses_map" "@" variable in out)) {
                    out["last_use_count"]++
                    out["last_uses_map", variable] = k
                    out["last_uses_list", out["last_use_count"], "variable"] = variable
                    out["last_uses_list", out["last_use_count"], "location"] = k
                }
            }

            if (tree_has_attr(s, instr, "operand2")) {
                variable = ir_get_operand2(s, instr)

                if (!("last_uses_map" "@" variable in out)) {
                    out["last_use_count"]++
                    out["last_uses_map", variable] = k
                    out["last_uses_list", out["last_use_count"], "variable"] = variable
                    out["last_uses_list", out["last_use_count"], "location"] = k
                }
            }

            k--
        }
    }
}

# active is a linked list of currently used registers sorted by their end time
function add_to_active(active, end, register,    curr) {
    if (active["head"] == -1) {
        active["head"] = register
        active[register, "end"] = end
        active[register, "next"] = -1
    }
    else {
        curr = active["head"]

        while (active[curr, "next"] != -1 && active[active[curr, "next"], "end"] < end) {
            curr = active[curr, "next"]
        }

        active[register, "end"] = end
        active[register, "next"] = active[curr, "next"]
        active[curr, "next"] = register
    }
}

function expire_old_intervals(free_registers, active, expiry_point,    curr) {
    curr = active["head"]

    while (curr != -1 && active[curr, "end"] < expiry_point) {
        active["head"] = active[curr, "next"]
        free_registers["count"]++
        free_registers[free_registers["count"]] = curr
        curr = active["head"]
    }
}

function linear_scan_register_allocation(s, funct, registers,    free_registers, usage, active, i) {
    find_usage(s, funct, usage)

    for (i = 1; i <= 11; i++) {
        free_registers[i] = "s" (12 - i)
    }

    free_registers["count"] = 11

    active["head"] = -1

    for (i = 1; i <= usage["first_use_count"]; i++) {
        expire_old_intervals(free_registers, active, usage["first_uses_list", i, "location"])

        if (free_registers["count"] <= 0) {
            fatal("Spill")
        }
        else {
            registers[usage["first_uses_list", i, "variable"]] = free_registers[free_registers["count"]]
            add_to_active(\
                active,
                usage["last_uses_map", usage["first_uses_list", i, "variable"]],
                free_registers[free_registers["count"]]\
            )
            free_registers["count"]--
        }
    }
}

function codegen(s, ir_root) {
    codegen_function(s, ir_root)
}

function codegen_function(s, l,    func_name, i, block, j, registers, var) {
    linear_scan_register_allocation(s, l, registers)
    func_name = tree_get_attr(s, l, "name")
    printf(".globl %s\n", func_name) > s["output_file"]

    printf("\n/*\n") > s["output_file"]

    for (var in registers) {
        printf("%s -> %s\n", var, registers[var]) > s["output_file"]
    }

    printf("*/\n\n") > s["output_file"]

    printf("%s:\n", func_name) > s["output_file"]

    for (i = 1; i <= tree_count_items(s, l); i++) {
        block = tree_get_item(s, l, i)
        printf("block.%d:\n", block) > s["output_file"]

        for (j = 1; j <= tree_count_items(s, block); j++) {
            codegen_instruction(s, tree_get_item(s, block, j), registers)
        }
    }
}

function codegen_instruction(s, l, registers,    type, dest, src1, src2, target, out) {
    out = s["output_file"]
    type = tree_get_type(s, l)

    if (tree_has_attr(s, l, "variable")) {
        dest = registers[ir_get_variable(s, l)]
    }

    if (tree_has_attr(s, l, "operand1")) {
        src1 = registers[ir_get_operand1(s, l)]
    }

    if (tree_has_attr(s, l, "operand2")) {
        src2 = registers[ir_get_operand2(s, l)]
    }

    if (tree_has_attr(s, l, "target")) {
        target = tree_get_attr(s, l, "target")
    }

    if (type == "return") {
        if (src1 != "a0") {
            printf("mv a0, %s\n", src1) > out
        }

        printf("ret\n") > out
    }
    else if (type == "constant") {
        printf("li %s, %d\n", dest, tree_get_attr(s, l, "value")) > out
    }
    else if (type == "negate") {
        printf("neg %s, %s\n", dest, src1) > out
    }
    else if (type == "logical_not") {
        printf("seqz %s, %s\n", dest, src1) > out
    }
    else if (type == "bitwise_not") {
        printf("not %s, %s\n", dest, src1) > out
    }
    else if (type == "mov") {
        printf("mv %s, %s\n", dest, src1) > out
    }
    else if (type == "add") {
        printf("add %s, %s, %s\n", dest, src1, src2) > out
    }
    else if (type == "sub") {
        printf("sub %s, %s, %s\n", dest, src1, src2) > out
    }
    else if (type == "mul") {
        printf("mul %s, %s, %s\n", dest, src1, src2) > out
    }
    else if (type == "div") {
        printf("div %s, %s, %s\n", dest, src1, src2) > out
    }
    else if (type == "mod") {
        printf("rem %s, %s, %s\n", dest, src1, src2) > out
    }
    else if (type == "jz") {
        printf("beq %s, x0, block.%d\n", src1, target) > out
    }
    else if (type == "jmp") {
        printf("j block.%d\n", target) > out
    }
    else {
        fatal(sprintf("can't codegen unknown instruction '%s'", type)) > out
    }
}
