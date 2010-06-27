

/* Do -real- prototype-based programming. */
if (typeof Object.beget !== 'function') {
	Object.beget = function(o) {
    	var F = function() {};
        F.prototype = o;
        return new F();
    };
}

x = { plain: "X", toString: function() { return " X "; } }
o = { plain: "O", toString: function() { return " O "; } }
x.other = o;
o.other = x;

space = {
	addr: 0,
	vacant: true,
	toString: function () { return "(" + this.addr + ")"; }
}

colSep = "|"
rowSep = "\n---+---+---\n"

makeBoard = function () {
	var sp = [];
	for (var i = 1; i <= 9; i++) {
		sp[i] = Object.beget(space);
		sp[i].addr = i;
	}
	return {
		spaces: sp,
		place: function (addr, player) {
			if (this.spaces[addr] && this.spaces[addr].vacant) {
				this.spaces[addr] = player;
				return true;
			} else {
				return false;
			}
		},
		toString: function () {
			return [
				this.spaces.slice(1,4).join(colSep),
				this.spaces.slice(4,7).join(colSep),
				this.spaces.slice(7,10).join(colSep)
			].join(rowSep)
		},
		full: function () {
			for (var i = 1; i <= 9; i++) {
				if (this.spaces[i].vacant) {
					return false;
				}
			}
			return true;
		},
		match3: function (x,y,z) {
			sp = this.spaces;
			if (sp[x] === sp[y] && sp[y] === sp[z]) {
				return sp[x];
			} else {
				return false;
			}
		},
		winner: function () {
			/* Taking advantage of short-circuit evaluation to
			 * bubble up the winner. */
			return this.match3(1,2,3) ||
				this.match3(4,5,6) ||
				this.match3(7,8,9) ||
				this.match3(1,4,7) ||
				this.match3(2,5,8) ||
				this.match3(3,6,9) ||
				this.match3(1,5,9) ||
				this.match3(3,5,7);
		}
	}
}

board = makeBoard();
player = x;

while (true) {
	print();
	print(board);
	print();
	maybe_winner = board.winner();
	if (maybe_winner) {
		print(maybe_winner.plain + " Wins!");
		break;
	} else if (board.full()) {
		print("It's a Draw!");
		break;
	} else {
		print("Select a square, " + player.plain + ": ");
		space = parseInt(readline());
		if (board.place(space, player)) {
			player = player.other;
		}
	}
}

