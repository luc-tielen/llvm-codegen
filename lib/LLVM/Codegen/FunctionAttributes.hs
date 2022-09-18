module LLVM.Codegen.FunctionAttributes (
    FunctionAttribute (..)
) where

import LLVM.Pretty (Pretty(pretty))

-- | Function attributes are set to communicate additional information about a function.
-- | Function attributes are considered to be part of the function, not of the function type,
-- | so functions with different function attributes can have the same function type.
data FunctionAttribute
    {- |
    This function attribute indicates that the function never returns normally,
    hence through a return instruction.

    This produces undefined behavior at runtime if the function ever does dynamically return.
    Annotated functions may still raise an exception, i.a., nounwind is not implied.
    -}
    = NoReturn
    {- |
    This function attribute indicates that the function never raises an exception.

    If the function does raise an exception, its runtime behavior is undefined.
    However, functions marked nounwind may still trap or generate asynchronous exceptions.
    Exception handling schemes that are recognized by LLVM to handle asynchronous exceptions,
    such as SEH, will still provide their implementation defined semantics.
    -}
    | NoUnwind
    {- |
    On a function, this attribute indicates that the function does not write through any pointer
    arguments (including byval arguments) or otherwise modify any state
    (e.g. memory, control registers, etc) visible outside the readonly function.

    It may dereference pointer arguments and read state that may be set in the caller.
    A readonly function always returns the same value (or unwinds an exception identically)
    when called with the same set of arguments and global state. This means while it cannot unwind
    exceptions by calling the C++ exception throwing methods (since they write to memory),
    there may be non-C++ mechanisms that throw exceptions without writing to LLVM visible memory.

    On an argument, this attribute indicates that the function does not write through this pointer
    argument, even though it may write to the memory that the pointer points to.

    If a readonly function writes memory visible outside the function, or has other side-effects,
    the behavior is undefined. If a function writes to a readonly pointer argument,
    the behavior is undefined.
    -}
    | ReadOnly
    {- |
    On a function, this attribute indicates that the function computes its result
    (or decides to unwind an exception) based strictly on its arguments, without dereferencing
    any pointer arguments or otherwise accessing any mutable state (e.g. memory, control registers, etc)
    visible outside the readnone function.

    It does not write through any pointer arguments (including byval arguments) and never changes any
    state visible to callers. This means while it cannot unwind exceptions by calling the C++ exception
    throwing methods (since they write to memory), there may be non-C++ mechanisms that throw exceptions
    without writing to LLVM visible memory.

    On an argument, this attribute indicates that the function does not dereference that pointer argument,
    even though it may read or write the memory that the pointer points to if accessed through other pointers.

    If a readnone function reads or writes memory visible outside the function, or has other side-effects,
    the behavior is undefined. If a function reads from or writes to a readnone pointer argument,
    the behavior is undefined.
    -}
    | ReadNone
    -- | This attribute indicates that the inliner should attempt to inline this function into callers
    -- | whenever possible, ignoring any active inlining size threshold for this caller.
    | AlwaysInline
    {- |
    This attribute indicates that calls to the function cannot be duplicated. A call to a noduplicate
    function may be moved within its parent function, but may not be duplicated within its parent function.

    A function containing a noduplicate call may still be an inlining candidate, provided that the call
    is not duplicated by inlining. That implies that the function has internal linkage and only has
    one call site, so the original call is dead after inlining.
    -}
    | NoDuplicate
    {- |
    In some parallel execution models, there exist operations that cannot be made control-dependent
    on any additional values. We call such operations convergent, and mark them with this attribute.

    The convergent attribute may appear on functions or call/invoke instructions.
    When it appears on a function, it indicates that calls to this function should not be made
    control-dependent on additional values. For example, the intrinsic llvm.nvvm.barrier0 is convergent,
    so calls to this intrinsic cannot be made control-dependent on additional values.

    When it appears on a call/invoke, the convergent attribute indicates that we should treat the call
    as though we’re calling a convergent function. This is particularly useful on indirect calls;
    without this we may treat such calls as though the target is non-convergent.

    The optimizer may remove the convergent attribute on functions when it can prove that the function
    does not execute any convergent operations. Similarly, the optimizer may remove convergent on
    calls/invokes when it can prove that the call/invoke cannot call a convergent function.
    -}
    | Convergent
    {- |
    This attribute indicates that the function may only access memory that is not accessible by the module
    being compiled before return from the function. This is a weaker form of readnone.
    If the function reads or writes other memory, the behavior is undefined.

    -- For clarity, note that such functions are allowed to return new memory which is noalias with
    respect to memory already accessible from the module. That is, a function can be both
    inaccessiblememonly and have a noalias return which introduces a new, potentially initialized, allocation.
    -}
    | InaccessibleMemory
    deriving stock (Eq, Ord, Show)

instance Pretty FunctionAttribute where
    pretty = \case
      NoReturn -> "noreturn"
      NoUnwind -> "nounwind"
      ReadOnly -> "readonly"
      ReadNone -> "readnone"
      AlwaysInline -> "alwaysinline"
      NoDuplicate -> "noduplicate"
      Convergent -> "convergent"
      InaccessibleMemory -> "inaccessiblememonly"
