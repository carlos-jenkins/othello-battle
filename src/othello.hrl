-record(game,
    {id      = othello, % constante
     black   = none,    % proceso que juega con negras (ie: <0.33.0>)
     white   = none,    % proceso que juega con blancas
     current = black,   % identificador de turno (white|black)
     pass    = 0,       % times in a row players have passed (< 2).
     seconds = 5,       % segundos que dura el turno
     timer   = none,    % identidad del proceso timer
     board   = none,    % tupla con el tablero actual
     border  = none     % lista con indices de posicion del borde
    }).
