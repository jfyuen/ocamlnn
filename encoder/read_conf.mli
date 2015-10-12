type file_contents =
  {
    layers : int array;
    activation : string;
    init : string;
    gn : bool;
    learning : string;
    threshold : float;
    vals : float array;
    max_iter : int;
    eps : float;
    stable_eps : float;
    stable_iter : int;
  }

val read_conf : string -> file_contents
