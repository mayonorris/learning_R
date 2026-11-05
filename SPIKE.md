# Test technique : webR sur téléphone

## Résultat

**Novembre 2026 : test concluant, webR est retenu.** La leçon de test passe
sur téléphone. Les mesures détaillées (téléphones, temps, données) restent à
reporter dans le tableau ci-dessous.

## La question à trancher

Une leçon où R tourne dans le navigateur (webR) est-elle utilisable sur un
téléphone d'entrée de gamme, avec un forfait mobile, au Togo et au Bénin ?
C'est le risque principal identifié dans le PRD. Il faut y répondre avant
d'écrire les 11 leçons du MVP.

## Mettre la page en ligne (une seule fois)

1. Sur GitHub, dans le dépôt `learning_R` : **Settings → Pages → Build and
   deployment → Source : « GitHub Actions »**.
2. Fusionner la pull request dans `main`. Le workflow « Publier le site » rend
   le site avec Quarto et le publie.
3. La leçon de test sera à l'adresse
   <https://mayonorris.github.io/learning_R/modules/01-initiation/1.2-vecteurs.html>.

## Matériel

- 2 ou 3 téléphones Android avec Chrome, dont au moins un d'entrée de gamme
  (2 Go de mémoire ou moins).
- Données mobiles 3G ou 4G, Wi-Fi coupé.
- Si possible, un ou deux étudiants pour faire l'exercice.

## Protocole, pour chaque téléphone

1. Couper le Wi-Fi. Noter la consommation de données de Chrome :
   **Paramètres → Réseau → Utilisation des données → Chrome** (le chemin
   varie selon les marques).
2. Ouvrir la leçon et ne rien toucher pendant 60 secondes. Noter de nouveau la
   consommation de Chrome. Si elle a beaucoup augmenté, webR se charge dès
   l'ouverture, sans attendre de clic.
3. Quand le bouton « Run Code » devient utilisable, toucher **R prêt** dans la
   barre de mesure en bas de l'écran.
4. Lancer le code de la section « À vous de lancer », puis toucher
   **Code lancé**.
5. Faire l'exercice au clavier du téléphone, sans l'indice si possible.
   Toucher **Exercice réussi** une fois validé. Noter les symboles difficiles
   à taper.
6. Toucher **Copier** et coller les mesures dans le tableau ci-dessous, ou
   dans WhatsApp.
7. Relever la consommation de Chrome : la différence avec l'étape 1 est le
   coût en données du premier passage.
8. Fermer l'onglet, rouvrir la leçon et refaire les étapes 3 à 7. C'est le
   second passage, avec le cache du navigateur.

Sur un ordinateur, on peut aussi ouvrir Chrome → outils de développement →
Réseau, cocher « Disable cache » et choisir « Slow 4G » : la liste montre tous
les fichiers de webR et leur taille totale.

## Résultats

| Téléphone (modèle, mémoire) | Réseau | Passage | Texte affiché (s) | R prêt (s) | Exercice réussi (s) | Données (Mo) | Remarques |
| --- | --- | --- | --- | --- | --- | --- | --- |
|  |  | 1er |  |  |  |  |  |
|  |  | 2e |  |  |  |  |  |
|  |  | 1er |  |  |  |  |  |
|  |  | 2e |  |  |  |  |  |

## Critères de décision (proposés, à ajuster)

On garde webR si, sur le téléphone d'entrée de gamme :

- le texte s'affiche en moins de 3 secondes ;
- R est prêt en moins de 30 secondes au premier passage et en moins de
  10 secondes au second ;
- l'onglet ne plante pas et l'exercice se fait au clavier en moins de
  3 minutes ;
- le coût en données du premier passage paraît acceptable aux étudiants
  interrogés.

## Si le test n'est pas concluant

- Charger R seulement à la demande, avec un avertissement qui annonce la
  taille à télécharger.
- Mode lecture par défaut sur téléphone ; exercices conseillés sur ordinateur.
- En dernier recours, exécuter R sur un serveur, avec un coût d'hébergement à
  prévoir.

## À observer pendant le test

- quarto-live charge-t-il webR dès l'ouverture de la page (étape 2) ?
- Les libellés de quarto-live (« Run Code », « Show Hint »…) peuvent-ils être
  traduits en français ?
- À quoi ressemble l'erreur de `note[2]`, et où brancher l'explication en
  français ?

## Limite de la barre de mesure

La barre ne voit pas les fichiers que webR télécharge : ils passent par un
« worker » du navigateur. La consommation de données se lit donc dans les
réglages du téléphone (étapes 1, 2 et 7).
