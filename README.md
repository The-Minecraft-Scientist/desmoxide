## Desmoxide
desmos, but its incredibly fast

### planned features
 * ASM JIT backend (compile your Desmos expressions to assembly!) via llvm with the `inkwell` crate
 * SPIR-V backend for GPU-side execution of expressions (with support for uniform variables and performant dynamically sized lists with buffers)
 * Optimize IR with Equality Saturation and the `egglog` crate (Unfortunately, Egglog's Rust API is basically nonexistent at the moment, so this is on hold until they actually implement one)
### current features
 * mostly feature-complete parser and AST structure for Desmos expressions ([link](https://github.com/The-Minecraft-Scientist/desmoxide/blob/master/src/ast/parser.rs))
 * First iteration of intermediate representation format implemented ([link](https://github.com/The-Minecraft-Scientist/desmoxide/blob/master/src/compile/ir.rs))
 * Lower Desmos expressions to IR ([link](https://github.com/The-Minecraft-Scientist/desmoxide/blob/master/src/compile/frontend.rs))
#### relevant design decisions
 * desmoxide (lib crate) should ***never*** panic. Ever. Calls to 'unwrap' and the likes are ONLY allowed when it is invariant that that unwrap will succeed. `anyhow` took away any excuses I had for panics/unwrap spam.
