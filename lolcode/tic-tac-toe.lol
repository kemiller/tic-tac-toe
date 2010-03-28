HAI
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

    LOL indeks IN MAH bord R playr
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

  OBTW
    You have to assign to nothing in order 
    to call a function that doesn't return 
    anything
  TLDR

  I HAS A fantzyBord ITZ printz YR bord MKAY
  VISIBLE fantzyBord

  I HAS A move

  VISIBLE "Select a square, X: "!
  GIMMEH NUMBR move

  I HAS A moveWerked ITZ makezYrMove YR "X" AN YR move AN YR bord MKAY

  LOL fantzyBord R printz YR bord MKAY
  VISIBLE fantzyBord

KTHXBYE

BTW vim: ts=2 sw=2 et
