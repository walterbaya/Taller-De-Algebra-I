caminos :: Tablero -> Camino -> [Camino]
caminos m n = eliminarepetidos(caminosextendidos  m n)

todosLosCaminos :: Tablero -> Conjunto(Camino)
todosLosCaminos m = eliminarepetidos(todosLosCaminos1 m)

caminosIndividuales :: Tablero -> [Camino]
caminosIndividuales m = individuales(coordenadas m )