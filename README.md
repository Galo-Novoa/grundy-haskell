# grundy-haskell
Juego de separar monedas en filas en Haskell para nuestra materia de Algoritmos.
<br>
## Integrantes del grupo
- Leanza, Tobías Gabriel
- Luján, Tomás
- Morales, Joaquín Francisco
- Novoa, Galo Cristian

## Instalación de Dependencias

Para utilizar la función de limpieza de pantalla, necesitas instalar el paquete `ansi-terminal`:

### En sistemas con Cabal:
```bash
cabal update
cabal install --lib ansi-terminal
```

### En sistemas con Stack:
```bash
stack install ansi-terminal
```

Una vez instalado el paquete, en GHCI:
```bash
ghci -package ansi-terminal main.hs
```

**Nota:** Si no puedes instalar `ansi-terminal`, el juego funcionará igual pero sin la función de limpieza de pantalla entre turnos