# Crear y compartir PhenoImage en GitHub

Esta guía está preparada para una cuenta nueva y un repositorio privado. El repositorio todavía no está creado.

## 1. Crear tu cuenta

1. Abre https://github.com/signup .
2. Crea una cuenta con tu correo, una contraseña propia y un nombre de usuario disponible.
3. Completa la verificación que solicite GitHub.
4. Conserva el nombre de usuario: será el propietario del repositorio. No necesitas compartir tu contraseña ni códigos de verificación para que preparemos el proyecto.

## 2. Crear el repositorio privado

1. Inicia sesión y abre https://github.com/new .
2. En **Owner**, selecciona tu cuenta.
3. En **Repository name**, escribe **PhenoImage**.
4. Descripción sugerida: **Fenotipado de forrajes mediante imágenes UAV · CIPAT · Grupo Papalotla**.
5. Selecciona **Private**.
6. Deja desmarcadas las opciones para agregar README, .gitignore o licencia: ya hay archivos preparados.
7. Pulsa **Create repository**.

## 3. Subir los archivos preparados

1. Descomprime el kit que te entregamos.
2. Dentro del repositorio vacío selecciona **uploading an existing file**, o **Add file → Upload files**.
3. Arrastra el CONTENIDO de la carpeta `PhenoImage` del kit. Incluye carpetas como `R`, `man`, `inst` y los archivos `DESCRIPTION`, `NAMESPACE`, `README.md`, `.gitignore` y `.Rbuildignore`.
4. Comprueba que `DESCRIPTION` y `README.md` quedan directamente en la raíz del repositorio, no dentro de otra carpeta `PhenoImage/`.
5. Mensaje de cambio sugerido: **Primera versión privada de PhenoImage E22**.
6. Pulsa **Commit changes**.

No subas el ZIP entero como sustituto del código. No subas ortomosaicos ni resultados a este historial. El kit separa esos datos del código para evitar archivos demasiado grandes en Git.

## 4. Crear una Release privada (descargas)

1. En el repositorio abre **Releases → Create a new release**.
2. Crea la etiqueta **v4.7.0.22**.
3. Título: **PhenoImage E22 · Evaluación privada**.
4. En la descripción indica que incluye instalación independiente, módulos Sentera/RGB y ejemplos opcionales. Se puede copiar el contenido de `NOTAS_RELEASE.md` de este kit.
5. Adjunta `phenoImage_4.7.0.22.tar.gz`, que viene preparado en el kit.
6. Opcionalmente adjunta los ZIP de ejemplo generados en el siguiente paso.
7. Publica la Release en el repositorio privado. Solo los usuarios con acceso podrán verla y descargarla.

## 5. Generar los ZIP de ejemplo en tu computadora

Tus ejemplos originales están en tu instalación anterior de CIPAT/PhenoImage o en su carpeta de datos. No hay que descargarlos otra vez.

1. Crea una carpeta vacía para las descargas de la Release y dentro un archivo TXT vacío, por ejemplo `destino.txt`.
2. Abre `GENERAR_EJEMPLOS.R` en RStudio y presiona Source.
3. En el selector elige ese TXT para señalar la carpeta de destino.
4. El script busca los datos existentes y genera `PhenoImage_Ejemplo_RGB.zip` y `PhenoImage_Ejemplo_Sentera.zip` si los encuentra completos.
5. Adjunta esos ZIP a la Release; no los agregues al historial del repositorio.

El script no sube ni publica nada automáticamente. Si falta un ejemplo, lo informa y puedes publicarlo después.

## 6. Invitar a tus colaboradores

1. En **Settings → Collaborators** (o la sección de acceso equivalente), agrega a la persona con su usuario de GitHub.
2. Debe aceptar la invitación.
3. Comparte la dirección real de tu repositorio; desde ahí podrá descargar e instalar el programa siguiendo el README.

Revisa los permisos al invitar: la colaboración en un repositorio personal puede permitir modificar el proyecto; si solo deseas distribuir archivos a lectores, habrá que definir una organización o un canal de distribución adecuado para ese acceso.

## 7. Antes de abrirlo al público

Revisa la autoría, el correo del mantenedor, la licencia y la autorización para distribuir ejemplos, logos y código. Mantenerlo privado inicialmente permite revisar estos puntos sin cambiar su visibilidad.

## Referencias oficiales

- Crear repositorio: https://docs.github.com/en/repositories/creating-and-managing-repositories/creating-a-new-repository
- Archivos grandes: https://docs.github.com/en/repositories/working-with-files/managing-large-files/about-large-files-on-github
- Releases: https://docs.github.com/en/repositories/releasing-projects-on-github/about-releases
