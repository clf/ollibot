structure StringMap = SplayMapFn(type ord_key = string 
                                 val compare = String.compare)
