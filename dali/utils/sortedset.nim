
type SortedSet*[T] = distinct seq[T]

proc init*[T](s: var SortedSet[T]) {.inline.} = s = SortedSet[T](newSeq[T]())

proc incl*[T](s: var SortedSet[T], item: T) =
  let i = s.search(item)
  if i == s.len:
    seq[T](s).add(item)
  elif item < s[i]:
    seq[T](s).insert(item, i)

proc `[]`*[T](s: SortedSet[T], i: int): T {.inline.} = seq[T](s)[i]

proc len*[T](s: SortedSet[T]): int {.inline.} = seq[T](s).len

proc search*[T](s: SortedSet[T], item: T): int =
  var i = s.len
  while result < i:
    let mid = (result + i) shr 1
    if item < s[mid]:
      i = mid
    elif s[mid] < item:
      result = mid + 1
    else:
      return mid

iterator items*[T](s: SortedSet[T]): T =
  for item in seq[T](s):
    yield item
