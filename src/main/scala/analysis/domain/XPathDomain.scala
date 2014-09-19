package analysis.domain

import xpath._

trait XPathDomain[T] {
   def top: T
   def bottom: T

   def join(v1: T, v2: T): T
   def meet(v1: T, v2: T): T

   def lift(v: XPathValue): T
 }