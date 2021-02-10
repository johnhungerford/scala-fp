package org.hungerford.fp.collections

import org.hungerford.fp.types.MonoidTest

class FpStringMonoidTest extends MonoidTest[ FpString ](
    "FpString",
    FpString,
    List(
      "one",
      "two",
      "three",
      "four",
    ),
) {

}
