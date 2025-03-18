#!/bin/bash
#Uso: $1 nombre de libreria $2 2 nombre del lst a generar

# Limpieza y creacion de las listas
rm -f notfound.lst
rm -f $2

touch notfound.lst
touch $2

# Buscar cada dependencia de ar
for file in $(ar -t $1); do
    name="${file%.*}"
    dependencies=$(find ../adainclude/ -maxdepth 1  -name "$name.*")

    if  [[ -z "${dependencies//[[:space:]]/}" ]]; then
	echo "$name" >> notfound.lst 
    else
	for dependency in $dependencies; do
	    nameExtension=$(basename "$dependency")
            echo "$nameExtension" >> $2
    	done
		
    fi
done

echo "Procesado de la libreria $1 terminado, los archivos no encontrados estan en notfound.lst"
