import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Queue;
import java.util.Set;

class TicTacToe {

	Queue<Player> players;
	Board board;
	BufferedReader in;

	public static void main(String argv[]) {
		new TicTacToe().play();
	}
	
	public TicTacToe() {
		// Player List 
		players = new LinkedList<Player>();
		players.add(Player.X);
		players.add(Player.O);
		
		board = new Board();
		
		in = new BufferedReader(new InputStreamReader(System.in));
	}
	
	public void play() {
		Player player = nextPlayer(); 
		while (true) {
			System.out.println("");
			System.out.println(board);
			if (board.winner() != null) {
				System.out.println(String.format("\n%s Wins!", board.winner()));
				break;
			} else if (board.full()) {
				System.out.println("\nIt's a Draw!");
				break;
			} else {
				System.out.print(String.format("\nSelect a square, %s: ", player));
				int move = 0;

				try {
					String answer = in.readLine();
					move = Integer.parseInt(answer);
				} catch (IOException e) {
					e.printStackTrace();
					System.exit(1);
				} catch (NumberFormatException e) {
					continue;
				}

				if (board.place(player, move)) { 
					player = nextPlayer();
				}
			}
		}
	}
	
	public Player nextPlayer() {
		Player nextPlayer = players.remove();
		players.add(nextPlayer);
		return nextPlayer;
	}

	class Cell {
		Player player = null;
		int address;
		List<List<Cell>> neighborLists;
		
		public Cell(int address) {
			super();
			this.address = address;
			this.neighborLists = new LinkedList<List<Cell>>();
		}
		
		public boolean assignPlayer(Player player, int targetAddress) {
			if (address == targetAddress) {
				this.player = player;
				return true;
			} else {
				return false;
			}
		}
		
		public void addNeighborList(List<Cell> list) {
			neighborLists.add(list);
		}
		
		public void addNeighbors(Cell first, Cell second) {
			addNeighborList(Arrays.asList(first, second));
		}
		
		public boolean isWinner() {
			for (List<Cell> list : neighborLists) {
				boolean result = true;
				for (Cell cell : list) {
					result = result && this.equals(cell);
				}
				if (result) return true;
			}
			return false;
		}

		@Override
		public String toString() {
			if (player == null) {
				return String.format("(%d)", address);
			} else {
				return String.format(" %s ", player);
			}
		}

		@Override
		public boolean equals(Object obj) {
			if (obj.getClass() == Cell.class) {
				return this.player == ((Cell) obj).player;
			} else {
				return false;
			}
		}
		
	}

	enum Player {
		X("X"),
		O("O");
		
		String name;
		
		Player(String name) {
			this.name = name;
		}

		public String toString() {
			return name;
		}
	}
	
	class Board {
		DelimitedList<DelimitedList<Cell>> cells;
		Set<Cell> freeCells;
		Player winner;
		
		private final static String COLSEP = "|";
		private final static String ROWSEP = "\n---+---+---\n";
		private static final int SIZE = 3;
		
		public Board() {
			
			// Lay out cells in the board
			freeCells = new HashSet<Cell>();
			cells = new DelimitedList<DelimitedList<Cell>>(ROWSEP);
			for (int i = 0; i < SIZE; i++) {
				DelimitedList<Cell> row = new DelimitedList<Cell>(COLSEP);
				cells.add(row);
				for (int j = 0; j < SIZE; j++) {
					Cell cell = new Cell(i*SIZE + j + 1);
					row.add(cell);
					freeCells.add(cell);
				}
			}
			
			defineRow(1, 2, 3);
			defineRow(4, 5, 6);
			defineRow(7, 8, 9);
			
			defineRow(1, 4, 7);
			defineRow(2, 5, 8);
			defineRow(3, 6, 9);
			
			defineRow(1, 5, 9);
			defineRow(3, 5, 7);
		}
		
		public String toString() {
			StringBuffer sb = new StringBuffer();
			display(sb);
			return sb.toString();
		}
		
		public void display(StringBuffer sb) {
			cells.setStringBuffer(sb);
			for (DelimitedList<Cell> row : cells) {
				display(row, sb);
			}
		}
		
		public void display(DelimitedList<Cell> row, StringBuffer sb) {
			row.setStringBuffer(sb);
			for (Cell cell : row) {
				sb.append(cell);
			}
		}
		
		public boolean full() {
			return freeCells.isEmpty();
		}
		
		public Player winner() {
			return this.winner;
		}
		
		public boolean complete() {
			return winner() != null || full();
		}
		
		public boolean place(Player player, int space) {
			Iterator<Cell> iterator = freeCells.iterator();
			while (iterator.hasNext()) {
				Cell cell = iterator.next();
				if (cell.assignPlayer(player, space)) {
					iterator.remove();
					if (cell.isWinner()) this.winner = player;
					return true;
				}
			}
			return false;
		}
		
		private Cell cellAt(int oneBasedIndex) {
			int i = oneBasedIndex  - 1;
			return cells.get(i / SIZE).get(i % SIZE);
		}
		
		private void defineRow(int first, int second, int third) {
			cellAt(first).addNeighbors(cellAt(second), cellAt(third));
			cellAt(second).addNeighbors(cellAt(third), cellAt(first));
			cellAt(third).addNeighbors(cellAt(first), cellAt(second));
		}
		
	}
	
	private static class DelimitingIterator<E> implements Iterator<E> {

		public DelimitingIterator(ListIterator<E> iterator,
				StringBuffer stringBuffer, String delimiter) {
			super();
			this.iterator = iterator;
			this.stringBuffer = stringBuffer;
			this.delimiter = delimiter;
		}

		ListIterator<E> iterator;
		StringBuffer stringBuffer;
		String delimiter;
		
		@Override
		public boolean hasNext() {
			return iterator.hasNext();
		}

		@Override
		public E next() {
			E nextItem =  iterator.next();
			if (iterator.previousIndex() != 0 && stringBuffer != null) {
				stringBuffer.append(delimiter);
			}
			return nextItem;
		}

		@Override
		public void remove() {
			iterator.remove();
		}

	}
	
	private static class DelimitedList<E> extends ArrayList<E> {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public DelimitedList(String delimiter) {
			super();
			this.delimiter = delimiter;
		}

		StringBuffer stringBuffer;
		String delimiter;
		
		public void setStringBuffer(StringBuffer stringBuffer) {
			this.stringBuffer = stringBuffer;
		}

		@Override
		public Iterator<E> iterator() {
			return new DelimitingIterator<E>(listIterator(), stringBuffer, delimiter);
		}

	}



}


