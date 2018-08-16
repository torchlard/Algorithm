-- Informatik 3, Hagerupp, Universität Augsburg, Übungsaufgabe 5.4

module AVLTreeExample where

import AVLTree

example :: AVLTree Int
example = Node 64 MinusOne
  (Node 20 PlusOne
    (Node 9 MinusOne
      (Node 4 PlusOne
        empty
        (singleton 7))
      (singleton 17))
    (Node 33 PlusOne
      (Node 28 MinusOne
        (singleton 23)
        empty)
      (Node 41 PlusOne
        (singleton 37)
        (Node 51 Zero
          (singleton 43)
          (singleton 55)))))
  (Node 75 PlusOne
    (Node 69 PlusOne
      empty
      (singleton 71))
    (Node 88 PlusOne
      (singleton 81)
      (Node 92 PlusOne
        empty
        (singleton 98))))

-- checkInorder example
-- checkBalanced example


