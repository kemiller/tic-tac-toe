

/* Do -real- prototype-based programming. */
if (typeof Object.beget !== 'function') {
	Object.beget = function(o) {
    	var F = function() {};
        F.prototype = o;
        return new F();
    };
}

x = { toString: function() { return " X "; } }
o = { toString: function() { return " O "; } }
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
		}
	}
}

board = makeBoard();

print(board);
