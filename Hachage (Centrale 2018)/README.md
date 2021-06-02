## [EXO] Table de hachage (Centrale 2018)

Une structure de dictionnaire est un ensemble de couples (clé, élément), les clés (nécessairement distinctes)
appartenant à un même ensemble 𝐾, les éléments à un ensemble 𝐸. La structure doit garantir les opérations
suivantes :

- recherche d’un élément connaissant sa clé ;
- ajout d’un couple (clé, élément) ;
- suppression d’un couple connaissant sa clé.

Une structure de dictionnaire peut-être réalisée à l’aide d’une table de hachage. Cette table est implantée dans
un tableau de 𝑤 listes (appelées alvéoles) de couples (clé, élément). Ce tableau est organisé de façon à ce que la
liste d’indice 𝑖 contienne tous les couples (𝑘, 𝑒) tels que ℎ𝑤(𝑘) = 𝑖 où ℎ𝑤 : 𝐾 → ⟦0, 𝑤 − 1⟧ s’appelle fonction de
hachage. On appelle 𝑤 la largeur de la table de hachage et ℎ𝑤(𝑘) le haché de la clé 𝑘.
Ainsi pour rechercher ou supprimer l’élément de clé 𝑘, on commence par calculer son haché qui détermine
l’alvéole adéquate et on est alors ramené à une action sur la liste correspondante. De même pour ajouter un
nouvel élément au dictionnaire on l’ajoute à l’alvéole indiquée par le haché de sa clé.
