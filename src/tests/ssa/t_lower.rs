use crate::context::ParsedContext;
use crate::elaborate::elaborate;
use crate::lexer::{LexError, Lexer, Token};
use crate::normalize::normalize;
use crate::parse::Parser;
use crate::resolve::resolve;
use crate::semck::sem_check;
use crate::ssa::lower::lower_func;
use crate::ssa::model::format::formact_func;
use crate::tree::semantic::MethodItem;
use crate::typeck::type_check;
use indoc::indoc;

fn analyze(source: &str) -> crate::context::SemanticContext {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");

    let mut parser = Parser::new(&tokens);
    let module = parser.parse().expect("Failed to parse");
    let id_gen = parser.into_id_gen();

    let ast_context = ParsedContext::new(module, id_gen);
    let resolved_context = resolve(ast_context).expect("Failed to resolve");
    let type_checked_context = type_check(resolved_context).expect("Failed to type check");
    let normalized_context = normalize(type_checked_context);
    let sem_checked_context = sem_check(normalized_context).expect("Failed to semantic check");
    elaborate(sem_checked_context)
}

#[test]
fn test_lower_const_return() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            42
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 42:u64
            ret %v0
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_return_stmt() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            return 42;
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 42:u64
            ret %v0
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_binop_return() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            1 + 2
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 1:u64
            %v1: u64 = const 2:u64
            %v2: u64 = add %v0, %v1
            ret %v2
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_param_binop_return() {
    let ctx = analyze(indoc! {"
        fn main(a: u64, b: u64) -> u64 {
            a + b
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main(u64, u64) -> u64 {
          bb0(%v0: u64, %v1: u64):
            %v2: u64 = add %v0, %v1
            ret %v2
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_call_with_params() {
    let ctx = analyze(indoc! {"
        fn add(a: u64, b: u64) -> u64 {
            a + b
        }

        fn main() -> u64 {
            add(1, 2)
        }
    "});
    let add_def = ctx.module.func_defs()[0];
    let main_def = ctx.module.func_defs()[1];
    let add_id = add_def.def_id;

    let lowered = lower_func(main_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = format!(
        indoc! {"
            fn main() -> u64 {{
              bb0():
                %v0: u64 = const 1:u64
                %v1: u64 = const 2:u64
                %v2: u64 = call @{}(%v0, %v1)
                ret %v2
            }}
        "},
        add_id
    );
    assert_eq!(text, expected);
}

#[test]
fn test_lower_call_stmt() {
    let ctx = analyze(indoc! {"
        fn add(a: u64, b: u64) -> u64 {
            a + b
        }

        fn main() -> u64 {
            add(1, 2);
            3
        }
    "});
    let add_def = ctx.module.func_defs()[0];
    let main_def = ctx.module.func_defs()[1];
    let add_id = add_def.def_id;

    let lowered = lower_func(main_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = format!(
        indoc! {"
            fn main() -> u64 {{
              bb0():
                %v0: u64 = const 1:u64
                %v1: u64 = const 2:u64
                %v2: u64 = call @{}(%v0, %v1)
                %v3: u64 = const 3:u64
                ret %v3
            }}
        "},
        add_id
    );
    assert_eq!(text, expected);
}

#[test]
fn test_lower_method_call_param() {
    let ctx = analyze(indoc! {"
        type Pair = { a: u64, b: u64 }

        Pair :: {
            fn sum(self) -> u64 {
                self.a + self.b
            }
        }

        fn main(p: Pair) -> u64 {
            p.sum()
        }
    "});

    let method_def = ctx
        .module
        .method_blocks()
        .iter()
        .flat_map(|block| block.method_items.iter())
        .find_map(|item| match item {
            MethodItem::Def(def) => Some(def),
            MethodItem::Decl(_) => None,
        })
        .expect("missing method def");
    let method_def_id = method_def.def_id;

    let main_def = ctx.module.func_defs()[0];

    let lowered = lower_func(main_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = format!(
        indoc! {"
            fn main(Pair) -> u64 {{
              bb0(%v0: Pair):
                %v1: u64 = call @{}(%v0)
                ret %v1
            }}
        "},
        method_def_id
    );
    assert_eq!(text, expected);
}

#[test]
fn test_lower_unop_return() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            ~1
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 1:u64
            %v1: u64 = bitnot %v0
            ret %v1
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_unop_not_return() {
    let ctx = analyze(indoc! {"
        fn main() -> bool {
            !true
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> bool {
          bb0():
            %v0: bool = const true
            %v1: bool = not %v0
            ret %v1
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_cmp_return() {
    let ctx = analyze(indoc! {"
        fn main() -> bool {
            1 < 2
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> bool {
          bb0():
            %v0: u64 = const 1:u64
            %v1: u64 = const 2:u64
            %v2: bool = cmp.lt %v0, %v1
            ret %v2
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_if_return() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            if true {
                1
            } else {
                2
            }
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: bool = const true
            cbr %v0, bb1, bb2

          bb1():
            %v2: u64 = const 1:u64
            br bb3(%v2)

          bb2():
            %v3: u64 = const 2:u64
            br bb3(%v3)

          bb3(%v1: u64):
            ret %v1
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_if_return_stmt() {
    let ctx = analyze(indoc! {"
        fn main() -> () {
            if true {
                return;
            } else {
                return;
            }
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> () {
          bb0():
            %v0: bool = const true
            cbr %v0, bb1, bb2

          bb1():
            ret

          bb2():
            ret

          bb3(%v1: ()):
            unreachable
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_if_stmt_side_effect() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            var i = 0;
            if true {
                return 1;
            } else {
                i = i + 1;
            };
            i
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 0:u64
            %v1: bool = const true
            cbr %v1, bb1, bb2

          bb1():
            %v4: u64 = const 1:u64
            ret %v4

          bb2():
            %v5: u64 = const 1:u64
            %v6: u64 = add %v0, %v5
            %v7: () = const ()
            br bb3(%v7, %v6)

          bb3(%v2: (), %v3: u64):
            ret %v3
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_if_cmp_return() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            if 1 < 2 {
                3
            } else {
                4
            }
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 1:u64
            %v1: u64 = const 2:u64
            %v2: bool = cmp.lt %v0, %v1
            cbr %v2, bb1, bb2

          bb1():
            %v4: u64 = const 3:u64
            br bb3(%v4)

          bb2():
            %v5: u64 = const 4:u64
            br bb3(%v5)

          bb3(%v3: u64):
            ret %v3
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_while_stmt() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            var i = 0;
            while i < 2 {
                i = i + 1;
            }
            i
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 0:u64
            br bb1(%v0)

          bb1(%v1: u64):
            %v3: u64 = const 2:u64
            %v4: bool = cmp.lt %v1, %v3
            cbr %v4, bb2, bb3(%v1)

          bb2():
            %v5: u64 = const 1:u64
            %v6: u64 = add %v1, %v5
            %v7: () = const ()
            br bb1(%v6)

          bb3(%v2: u64):
            ret %v2
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_while_return_stmt() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            var i = 0;
            while i < 2 {
                return 1;
            }
            2
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 0:u64
            br bb1(%v0)

          bb1(%v1: u64):
            %v3: u64 = const 2:u64
            %v4: bool = cmp.lt %v1, %v3
            cbr %v4, bb2, bb3(%v1)

          bb2():
            %v5: u64 = const 1:u64
            ret %v5

          bb3(%v2: u64):
            %v6: u64 = const 2:u64
            ret %v6
        }
    "};
    assert_eq!(text, expected);
}
