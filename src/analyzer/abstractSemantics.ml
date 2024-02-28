module type S = 
  sig
    type memty
    val transfer : Basicblock.t ->  memty ->  memty
    val abs_interp_global : Global.t -> memty -> memty
  end
