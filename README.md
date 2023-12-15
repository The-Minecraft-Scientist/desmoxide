## Desmoxide
desmos, but its incredibly fast

### planned features
 * parse and lower desmos expressions to performance-centric, statically typed intermediate representation
 * ASM JIT backend (compile your Desmos expressions to assembly!) with the `inkwell` crate
 * SPIR-V backend for GPU-side execution of expressions (with support for uniform variables and performant dynamically sized lists with buffers)
 * Optimize IR with Equality Saturation and the `egg` crate
### current features
 * mostly feature-complete parser and AST structure for Desmos expressions (see [here](https://github.com/The-Minecraft-Scientist/desmoxide/blob/master/src/ast/parse_manager.rs))
 * First iteration of intermediate representation format implemented (see [here](https://github.com/The-Minecraft-Scientist/desmoxide/blob/master/src/compile/ir/mod.rs))
