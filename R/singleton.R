#
# Represent an object on which we cannot set attributes,
# primarily because there cane be only one of these, e.g.,
# NULL
# emptyenv()
# base environment
# symbol

# Also, shouldn't be assigning values such as offset into

# We could also wrap every object into a info + value

wrap =
function(x, type, offset, ...)
{
    ans = data.frame(type = type, offset = offset, ...)
    ans$value = list(x)
    structure(ans, class = c("wrappedObject", "data.frame"))
}
