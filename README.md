## Использование

1. `ruz-finder ovd URL` - скачать список задержанных с ссылки `URL`, отфильтровать по РУЗу, найти возможные профили в ВК.
   Переменные среды:
   - `DATA_DIR` - директория с выводом программы, по умолчанию равна `.` (в ней нужно создать пустой файл `all.csv`)
2. `find_in_vk FULL_NAME` - найти ВК по фамилии-имени.
   ВАЖНО: ВК не отбрасывает отчество, а выдаёт совершенно другие результаты.
   Вывод программы: в 1 строке выведется progress bar, в последующих - строки вида `ссылка (тип совпадения)`
3. `find_in_vk IN_FILE OUT_FILE` - программа ожидает в каждой строке файла `IN_FILE` фамилию-имя, которые нужно найти в ВК.
   - Если `IN_FILE` равен `-`, вместо файла используется стандартный ввод.
   - Вывод в файл `OUT_FILE` в формате CSV: `ФИ,ссылка1 (тип совпадения),ссылка2 (тип совпадения),...`
   - Если `OUT_FILE` равен `-`, вместо файла используется стандартный вывод (progress bar тоже отрисовывается в stdout)

## Токен для доступа к ВК
Для работы каждой подпрограммы нужна переменная среды `VK_TOKEN`.
Получение:
- Перейти по https://oauth.vk.com/authorize?client_id=8094092&display=page&redirect_uri=https://oauth.vk.com/blank.html&scope=327682&response_type=token&v=5.131
- Из адресной строки скопировать access_token
