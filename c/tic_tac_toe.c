
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MIN_SPACE 1
#define MAX_SPACE 9

typedef enum PLAYER { EMPTY, X, O } player;
typedef player board[3][3];
typedef enum BOOL { false, true } bool;

#define SPACE(board, space) board[((space-1)/3)][(space-1)%3]

char P2C[3] = { ' ', 'X', 'O' }; 

/*
 * Player Functions
 */
player other_player(player player) {
	if (X == player) return O;
	if (O == player) return X;
	return EMPTY;
}

/*
 * Board functions
 */
bool space_available(board board, int space) {
	return SPACE(board, space) == EMPTY;
}

void make_move(board board, player player, int space) { 
	SPACE(board, space) = player;
}

bool triple_match(board board, int s1, int s2, int s3) {
	return SPACE(board, s1) != EMPTY 
		&& SPACE(board, s1) == SPACE(board, s2)
		&& SPACE(board, s1) == SPACE(board, s3);
}

bool winner(board board) {
	return triple_match(board, 1, 2, 3) /* rows */
		|| triple_match(board, 4, 5, 6)
		|| triple_match(board, 7, 8, 9)

		|| triple_match(board, 1, 4, 7) /* columns */
		|| triple_match(board, 2, 5, 8)
		|| triple_match(board, 3, 6, 9)

		|| triple_match(board, 1, 5, 9) /* diagonals */
		|| triple_match(board, 3, 5, 7);
}

bool full(board board) {
	int s;

	for(s = 1; s <= 9; s++) {
		if (space_available(board, s)) {
			return false;
		}
	}
	return true;
}

/*
 * Output
 */

void print_space(player space, int x, int y) {
	if (EMPTY == space) {
		printf("(%d)", (y*3) + x + 1);
	} else {
		printf(" %c ", P2C[space]);
	} 
}

void print_board(board board) {

	int x, y;

	putchar('\n');
	for (y = 0; y < 3; y++) {
		for (x = 0; x < 3; x++) {
			print_space(board[y][x], x, y);
			if (x < 2) putchar('|');
		}
		if (y < 2) puts("\n---+---+---");
	}
	putchar('\n');
	putchar('\n');
}

/*
 * Input
 */
void read_space(int * s) {
	int rcode;
	rcode = scanf("%d", s);
	if (EOF == rcode) {
		exit(1);
	} else if (rcode <= 0) {
		*s = -1;
	}
}

/*
 * Main function
 */
int main(char * argv[], int argc) {
	board board;
	player current_player = X;
	int space;

	memset(board, EMPTY, sizeof(board));

	while (true) {
		print_board(board);

		if (winner(board)) {
			return printf("%c Wins!\n", P2C[other_player(current_player)]) <= 0;
		} else if (full(board)) {
			return printf("It's a Draw!\n") <= 0;
		} else {
			printf("Select a square, %c: ", P2C[current_player]);
		}

		read_space(&space);

		if (space >= MIN_SPACE && space <= MAX_SPACE && space_available(board, space)) {
			make_move(board, current_player, space); 
			current_player = other_player(current_player);
		}
	}

	return 0;
}

