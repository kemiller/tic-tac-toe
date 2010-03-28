HAI

  OBTW
    join
    Basic array join.
  TLDR	
  HOW DUZ I join YR a AN YR b
    I HAS A count ITZ ALL a
    LOL count R count NERF 1

    IZ count SMALR DEN 0 ? FOUND YR ""

    I HAS A output ITZ ""
    IM IN YR LOOP
      LOL output R count IN MAH a N output

      IZ count BIGR DEN 0 ? LOL output R b N output
      IZ count LIEK 0 ? GTFO

      LOL count R count NERF 1
    KTHX

    FOUND YR output
  IF U SAY SO

  OBTW
    cell formatting
  TLDR
  HOW DUZ I formatz YR cell
    IZ cell SORTA "\d" ? FOUND YR "(" N cell N ")"
    FOUND YR " " N cell N " "
  IF U SAY SO

  HOW DUZ I printz YR bord
    I HAS A cell 
    I HAS A colsep ITZ "|"
    I HAS A rowsep ITZ "\n---+---+---\n"
    I HAS A outputz ITZ ""

    LOL cell R formatz YR 0 IN MAH bord MKAY
    LOL outputz R "\n" N cell N colsep

    LOL cell R formatz YR 1 IN MAH bord MKAY
    LOL outputz R outputz N  cell N colsep

    LOL cell R formatz YR 2 IN MAH bord MKAY
    LOL outputz R outputz N  cell N rowsep

    LOL cell R formatz YR 3 IN MAH bord MKAY
    LOL outputz R outputz N  cell N colsep

    LOL cell R formatz YR 4 IN MAH bord MKAY
    LOL outputz R outputz N  cell N colsep

    LOL cell R formatz YR 5 IN MAH bord MKAY
    LOL outputz R outputz N  cell N rowsep

    LOL cell R formatz YR 6 IN MAH bord MKAY
    LOL outputz R outputz N  cell N colsep

    LOL cell R formatz YR 7 IN MAH bord MKAY
    LOL outputz R outputz N  cell N colsep

    LOL cell R formatz YR 8 IN MAH bord MKAY
    LOL outputz R outputz N cell N "\n"

    FOUND YR outputz
  IF U SAY SO

  HOW DUZ I makezYrMove YR playr AN YR space AN YR bord
    BTW This version of LOLCode has no TROOF values, so I use NUMBRs

    IZ space SMALR DEN 1 ? FOUND YR 0
    IZ space BIGR DEN 9 ? FOUND YR 0

    I HAS A indeks ITZ space NERF 1

    IZ indeks IN MAH bord SORTA "\d" O RLY?
      YA RLY
        LOL indeks IN MAH bord R playr
        FOUND YR 1
      NO WAI
        FOUND YR 0
    KTHX
  IF U SAY SO

  HOW DUZ I canHasWinnarIn YR bord AN YR playr
    I HAS A bordMaidUvYarn ITZ join YR bord AN YR "" MKAY
    
    BTW rows
    IZ bordMaidUvYarn SORTA "(" N playr N ")(" N playr N ")(" N playr N ")......" ? FOUND YR 1
    IZ bordMaidUvYarn SORTA "...(" N playr N ")(" N playr N ")(" N playr N ")..." ? FOUND YR 1
    IZ bordMaidUvYarn SORTA "......(" N playr N ")(" N playr N ")(" N playr N ")" ? FOUND YR 1

    BTW columns
    IZ bordMaidUvYarn SORTA "(" N playr N ")..(" N playr N ")..(" N playr N ").." ? FOUND YR 1
    IZ bordMaidUvYarn SORTA ".(" N playr N ")..(" N playr N ")..(" N playr N ")." ? FOUND YR 1
    IZ bordMaidUvYarn SORTA "..(" N playr N ")..(" N playr N ")..(" N playr N ")" ? FOUND YR 1

    BTW diagonals
    IZ bordMaidUvYarn SORTA "(" N playr N ")...(" N playr N ")...(" N playr N ")" ? FOUND YR 1
    IZ bordMaidUvYarn SORTA "..(" N playr N ").(" N playr N ").(" N playr N ").." ? FOUND YR 1

    FOUND YR 0
  IF U SAY SO

  HOW DUZ I canHasTieIn YR fantsyBord
    IZ fantsyBord SORTA "\d" ? FOUND YR 0 
    FOUND YR 1
  IF U SAY SO
    
  I HAS A bord
  I HAS A bord_lemf ITZ 9
  I HAS A current_lemf
  IM IN YR loop
    LOL current_lemf R ALL bord
    IZ current_lemf LIEK bord_lemf ? GTFO

    LOL current_lemf IN MAH bord R current_lemf UP 1
  KTHX

  I HAS A playr ITZ "X"
  I HAS A moovWerked ITZ 0

  IM IN YR gaimLoop
    I HAS A fantsyBord ITZ printz YR bord MKAY
    VISIBLE fantsyBord

    I HAS A winnar ITZ canHasWinnarIn YR bord AN YR playr MKAY
    IZ winnar LIEK 1 O RLY?
      YA RLY
        VISIBLE playr N " IS TEH WINNAR!1!!!"
        BTW Use this line instead if you want to run tests
        BTW VISIBLE playr N " Wins!"
        GTFO
    KTHX

    I HAS A tie ITZ canHasTieIn YR fantsyBord MKAY
    IZ tie LIEK 1 O RLY?
      YA RLY
        VISIBLE "NO MOAR WINNARZ!!!?/1!"
        BTW Use this line instead if you want to run tests
        BTW VISIBLE "It's a Draw!"
        GTFO
    KTHX

    IZ moovWerked LIEK 1 O RLY?
      YA RLY
        IZ playr LIEK "X" O RLY?
          YA RLY
            LOL playr R "O"
          NO WAI
            LOL playr R "X"
        KTHX
    KTHX

    I HAS A moov

    VISIBLE "Makez yr moov, " N playr N ": "!
    BTW Use this line instead if you want to run tests
    BTW VISIBLE "Select a square, " N playr N ": "!

    GIMMEH NUMBR moov

    LOL moovWerked R makezYrMove YR playr AN YR moov AN YR bord MKAY
  KTHX

KTHXBYE

BTW vim: ts=2 sw=2 et
