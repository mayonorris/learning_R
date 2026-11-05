# Learning R

Plateforme de formation à R en français, d'abord pour le Togo et le Bénin.

## État actuel : test technique

Le dépôt contient un premier site [Quarto](https://quarto.org) avec une leçon de
test (`modules/01-initiation/1.2-vecteurs.qmd`). R y tourne dans le navigateur
grâce à [webR](https://docs.r-wasm.org/webr/) et à l'extension
[quarto-live](https://github.com/r-wasm/quarto-live). Le protocole du test sur
téléphone est décrit dans [`SPIKE.md`](SPIKE.md).

## Rendre le site sur son ordinateur

```bash
quarto add r-wasm/quarto-live
quarto preview
```

Le workflow `.github/workflows/publier.yml` fait la même chose sur GitHub et
publie le site sur GitHub Pages à chaque mise à jour de `main`.

## Scripts R

Les premiers exercices (`compute-mean.R`, `find-the_minimum.R`,
`matrices-product.R`, `simple.R`) serviront de matière aux leçons du module 1.
