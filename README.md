
`fisabio`
==========

Acerca de este proyecto
-----------------------

`fisabio` es un paquete de R cuyo único propósito es estructurar y organizar el entorno de trabajo, facilitando el trabajo en equipo dentro del Servicio de Estudios Estadísticos de FISABIO.

El proyecto generado con su función principal (`nuevo_proyecto()`) se estructura en 4 directorios, lo que permite seguir un criterio unificado a la hora de trabajar en equipo o, sencillamente, retomar un trabajo individual que se aparcase hace algún tiempo. El contenido de cada uno de estos directorios es:

+ r: directorio principal, el cual contiene todos los *scripts* de código R necesarios para la ejecución del proyecto. Solo contiene archivos con extensión `*.R`, cuyo nombre contiene dos dígitos al inicio que identifica el orden de ejecución de los mismos.
+ datos: donde se almacenan los bancos de datos necesarios para el análisis. Se estructura en tres subdirectorios:
    - brutos: datos **TAL CUAL** se reciben, sin importar su formato (***NUNCA SE MODIFICAN***).
    - procesados: datos procesados listos para cargar durante el análisis (generados tras ejecutar los *scripts* de lectura y procesado de datos). Preferentemente se utilizará un formato nativo de R (`*.RData`).
    - documentacion: directorio para guardar toda la documentación asociada al proyecto y facilitada por el cliente, incluyendo un diccionario de variables donde figuran las etiquetas de cada una de ellas.
+ informes: contenido a facilitar al cliente. En la primera reunión se le solicita el formato que se le debe dar, siendo las opciones ODT, PDF y Beamer para presentaciones.
+ presupuestos: directorio que contiene tanto los presupuestos como las notas de cargo o facturas.


A su vez, la función `nuevo_proyecto()` crea por defecto un repositorio Git y un sistema de control de versiones de paquetes de `R` empleando `renv`.

Gracias al conjunto de funciones `informe_*()` y `presentacion_*()`, se pueden generar informes y presentaciones en formatos ODT y PDF, todos ellos dentro del directorio `informes`.

Instalación
-----------

Puedes instalar este paquete empleando la función `install_github()` del paquete `remotes`, para lo cual debes ejecutar el siguiente código:

```
if (!"remotes" %in% installed.packages()) install.packages("remotes")
remotes::install_github("estadistica-fisabio/fisabio")
```


Acerca de `fisabio`
--------------------

`fisabio` es el nombre que nos hemos dado como grupo de usuarios de R dentro de la Fundación para el Fomento de la Investigación Sanitaria y Biomédica de la Comunidad Valenciana (FISABIO).

Si al igual que a nosotros te encanta la estadística en R, o si tuvieras alguna duda acerca del grupo o su actividad, puedes contactarnos sin problema a través del correo <estadistica_fisabio@gva.es>.
