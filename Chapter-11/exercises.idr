every_other : Stream ty -> Stream ty
every_other (skip :: value :: xs) = value :: every_other xs
