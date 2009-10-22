
require 'test/unit'
require 'expect'
require 'pty'

require File.join(File.dirname(__FILE__), 'boards.rb')

class TttTest < Test::Unit::TestCase

  include Boards

  EOF = 4.chr

  def test_starts_with_x
    assert_equal STARTING_BOARD, run_script(EOF)
  end

  def test_x_1
    assert_equal X_ON_1, run_script(1,EOF)
  end

  def test_x_5_o_9
    assert_equal X_ON_5_O_ON_9, run_script(5,9,EOF)
  end

  def test_x_wins_first_horiz_rank
    assert_equal X_WINS_1ST_HORIZ_RANK, run_script(1,4,2,5,3,nil)
  end

  def test_x_wins_second_horiz_rank
    assert_equal X_WINS_2ND_HORIZ_RANK, run_script(4,7,5,8,6,nil)
  end

  def test_x_wins_third_horiz_rank
    assert_equal X_WINS_3RD_HORIZ_RANK, run_script(7,4,8,5,9,nil)
  end

  def test_x_wins_first_vert_rank
    assert_equal X_WINS_1ST_VERT_RANK, run_script(1,2,4,5,7,nil)
  end

  def test_x_wins_second_vert_rank
    assert_equal X_WINS_2ND_VERT_RANK, run_script(2,1,5,6,8,nil)
  end

  def test_x_wins_third_vert_rank
    assert_equal X_WINS_3RD_VERT_RANK, run_script(3,1,6,4,9,nil)
  end

  def test_x_wins_slash_diag
    assert_equal X_WINS_SLASH_DIAG, run_script(7,1,5,9,3,nil)
  end

  def test_x_wins_backslash_diag
    assert_equal X_WINS_BACKSLASH_DIAG, run_script(9,7,1,3,5,nil)
  end

  def test_o_wins_first_horiz_rank
    assert_equal O_WINS_1ST_HORIZ_RANK, run_script(4,1,5,2,9,3,nil)
    assert_equal O_WINS_1ST_HORIZ_RANK, run_script(5,2,4,1,9,3,nil)
  end

  def test_draw_diagonal_y
    assert_equal DRAW_DIAGONAL_Y, run_script(1,5,7,4,6,2,8,9,3,nil)
  end

  def test_draw_vertical_y
    assert_equal DRAW_VERTICAL_Y, run_script(7,5,4,1,9,8,6,3,2,nil)
  end

  def test_unavailable_space
    assert_equal X_ON_1, run_script(1,1,EOF)
  end

  def test_bad_number
    assert_equal X_ON_5_O_ON_9, run_script(5,9,123,EOF)
  end

  protected

  # Runs the program, enters the input enumerable one by one, and records the last output
  def run_script(*inputs)

    ttt = ENV['TTT'] || "./ttt"
    last_response = nil
    penultimate_input = ''
    last_input = ''

    PTY.spawn(ttt) do |read, write, pid|
      write.sync = true

      inputs.each do |input|
        read.expect(/.*[:!]\s/, 1) do |response|
          return nil unless response
          last_response = response
          penultimate_input = last_input
          if input
            last_input = input.to_s + "\n"
            write.print last_input
          end
        end
      end
    end
    if last_response
      last_response.join('').gsub(/\r\n|\r/,"\n").gsub(/\A#{penultimate_input}/,'')
    end
  end
end
