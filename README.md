## Использование

1. `ruz-finder ovd URL` - скачать список задержанных с ссылки `URL`, отфильтровать по РУЗу, найти возможные профили в ВК
   Переменные среды:
   - `DATA_DIR` - директория с выводом программы (в ней нужно создать пустой файл `all.csv`)
2. `find_in_vk FULL_NAME` - найти ВК по фамилии-имени.
   ВАЖНО: ВК не отбрасывает отчество, а выдаёт совершенно другие результаты
   Вывод программы - в свободном варианте.
3. `find_in_vk - OUT_FILE` - программа ожидает в стандартном вводе строки, в каждой - фамилия-имя, которые нужно найти в ВК.
   Вывод в CSV: ФИ,ссылка1,ссылка2,...

## Токен для доступа к ВК
Для работы каждой подпрограммы нужна переменная среды `VK_TOKEN`.
Получение:
- Перейти по https://oauth.vk.com/authorize?client_id=8091308&display=page&redirect_uri=https://oauth.vk.com/blank.html&scope=friends&response_type=token&v=5.131
- Из адресной строки скопировать access_token