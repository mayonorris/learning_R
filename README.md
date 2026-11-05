# Learning R

Plateforme de formation à R en français, d'abord pour le Togo et le Bénin.

## Contenu

Le site est construit avec [Quarto](https://quarto.org). R tourne dans le
navigateur grâce à [webR](https://docs.r-wasm.org/webr/) et à l'extension
[quarto-live](https://github.com/r-wasm/quarto-live).

- `index.qmd`, `cursus.qmd` : accueil et catalogue des modules
- `progression.qmd`, `communaute.qmd` : ma progression, communauté et FAQ
- `modules/00-demarrer/` : module 0, 4 leçons
- `modules/01-initiation/` : module 1, 7 leçons
- `assets/theme.scss` : couleurs et typographie
- `assets/progression.html` : inclus dans chaque page ; progression et série de
  jours (stockées dans le navigateur), code de reprise, partage WhatsApp et
  erreurs R expliquées en français
- `assets/mesure.html` : barre de mesure du test technique (voir
  [`SPIKE.md`](SPIKE.md)), à inclure dans une page pour refaire un test

## Rendre le site sur son ordinateur

```bash
tmp="$(mktemp -d)" && (cd "$tmp" && quarto add r-wasm/quarto-live --no-prompt) && cp -r "$tmp/_extensions" .
quarto preview
```

L'extension s'installe hors du projet, puis se copie : lancée directement dans
le projet, `quarto add` échoue, car la leçon inclut déjà un fichier de
l'extension.

Le workflow `.github/workflows/publier.yml` fait la même chose sur GitHub et
publie le site sur GitHub Pages à chaque mise à jour de `main`.

## Scripts R

Les premiers exercices (`compute-mean.R`, `find-the_minimum.R`,
`matrices-product.R`, `simple.R`) serviront de matière aux leçons du module 1.
