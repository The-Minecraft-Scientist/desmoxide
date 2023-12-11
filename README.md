## Desmoxide
desmos, but its incredibly fast

### planned features
 * parse and lower desmos expressions to performance-centric, statically typed mid-level IR
 * ASM JIT backend (compile your Desmos expressions to assembly!) with the `inkwell` crate
 * SPIR-V backend for GPU-side execution of expressions (with support for uniform variables and performant dynamically sized lists with buffers)
 * Optimize IR with Equality Saturation and the `egg` crate
