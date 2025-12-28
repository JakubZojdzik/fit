# fit - functional git

## Dlaczego?

Niemutowalna natura commitów kojarzy się z modelem pamięci języków funkcyjnych.

## Cel

Program powinien działać jako narzędzie CLI o semantyce zbliżonej do gita. Byłoby super jakby działał zestaw komend:
(posortowane po priorytecie)
- `fit init`
- `fit config ...`
- `fit commit "Commit message"`
- `fit checkout <ID>`
- `fit merge <ID>`
- `fit restore <path>`
- `fit revert <ID>`

Nie przewiduję `fit reset`, `fit rebase` ani `fit revert`, bo to polecenia dla słabych programistów.
Alternatywą dla `reset` będzie `checkout`

## Wstępne rozważania

- Czy commit powinien przechowywać cały snapshot, tak jak git? Czy może lepiej trzymać samego diffa?
- Jeśli trzymać diffa, to czy używać np. unixowego `diff -b` czy implementować własnego?
- Jeśli trzymać snapshoty, to wszystkich? Czy szukać tylko zmienionych plików? Czy może tylko zmienionych kawałków plików?
- Jak zaimplementować merge? Poprostu 2 rodziców? A może zapożyczyć rozwiązanie z tfsa? Co z rozstrzyganiem konfliktów?
- Czy implementować stage? Czy może wszystko co jest w katalogu projektu już będzie w stage i wykluczać tylko pliki z .fitignore?
- Czy zipper nada się do poruszania się po strukturze commitów?
- Czy da się zoptymalizować wczytywanie całej historii czy przy każdym wywołaniu komendy fit trzeba robić to od nowa?

## Na przyszłość

- Komunikacja z innymi instancjami fita (remote)
- Globalny config
- Podpisywanie commitów (póki co można się podpisać w komentarzu w kodzie)
