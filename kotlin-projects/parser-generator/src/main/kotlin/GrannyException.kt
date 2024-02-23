sealed class GrannyException(override val message: String?) : Exception(message)

class LeftRecursionException : GrannyException(
    "Given grammar contains left recursion and thus cannot be LL(1)"
)

class NotLL1Grammar : GrannyException(
    "Given grammar does not meet LL(1) requirements"
)