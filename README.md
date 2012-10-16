About
=====

Othello-Battle is a 0 players Othello game written in Erlang made to test MiniMax 
algorithms against a known opponent with unknown implementation (client.beam).


Screenshots
===========

![Othello display](https://raw.github.com/carlos-jenkins/othello-battle/master/media/display.png "Othello display")

How to use
==========

- Compile the server module:  
    ``1> c(server.erl).``
- Run start() from the server module:  
    ``2> server:start().``
- Create and connect opponent:  
    ``3> client:start().``  
    ``4> client:connect(white).``
- Create and connect the known implementation:  
    ``3> clientk:start().``  
    ``4> clientk:connect(black).``
- Enjoy :)


License
=======

Copyright (C) 2012 Carlos Jenkins <carlos@jenkins.co.cr>  
Copyright (C) 2012 Pablo Rodriguez <pabloarb@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

