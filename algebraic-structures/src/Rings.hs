module Rings (
  Ring(Ring)
) where

import Groups

-- Rings contain the following elements: an abelian group `Group a`
data Ring a = Ring (Group a)