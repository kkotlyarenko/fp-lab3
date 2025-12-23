# Лабораторная работа №3

Студент: **Котляренко Кирилл Андреевич**  
Группа: **P3309**

## Описание

Программа для потоковой интерполяции данных с поддержкой двух алгоритмов:
- **Линейная интерполяция** - интерполяция отрезками между соседними точками
- **Интерполяция Ньютона** - полиномиальная интерполяция с использованием разделённых разностей


## Сборка

```bash
dune build
```

## Запуск

```bash
# Линейная интерполяция с шагом 0.5
echo -e "0 0\n1 1\n2 4\n3 9" | ./_build/default/bin/main.exe --linear --step 0.5

# Интерполяция Ньютона с 4 точками и шагом 0.5
echo -e "0 0\n1 1\n2 4\n3 9\n4 16" | ./_build/default/bin/main.exe --newton -n 4 --step 0.5

# Оба алгоритма одновременно
echo -e "0 0\n1 1\n2 4\n3 9" | ./_build/default/bin/main.exe --linear --newton -n 3 --step 0.5

# Пример CSV-подобного ввода
cat test_data.csv | ./_build/default/bin/main.exe --linear --newton -n 4 --step 1
```

## Опции командной строки

| Опция | Описание |
|-------|----------|
| `--linear` | Использовать линейную интерполяцию |
| `--newton` | Использовать интерполяцию Ньютона |
| `--step <float>` | Шаг дискретизации (по умолчанию 1.0) |
| `-n <int>` | Количество точек для интерполяции Ньютона (по умолчанию 4) |
| `--help` | Показать справку |


## Формат входных данных

Данные подаются на stdin в формате `x<sep>y`, где `<sep>` может быть:
- `;` (CSV-подобный формат)
- `,`
- табуляция
- пробел

Пример:
```
0 0
1 1
2 4
3 9
```

## Тестирование

```bash
dune runtest
```

### Вывод тестов

```text
Testing `linear'.                  
This run has ID `0X3LS214'.

  [OK]          linear          0   linear_interpolate.
  [OK]          linear          1   generate_points.
  [OK]          linear          2   linear_interpolation.

Full test results in `~/Documents/ITMO/3 course/Functional Programming/fp-lab3/_build/default/test/_build/_tests/linear'.
Test Successful in 0.001s. 3 tests run.
Testing `newton'.                  
This run has ID `O9DLJ9QI'.

  [OK]          newton          0   divided_differences.
  [OK]          newton          1   evaluate.
  [OK]          newton          2   interpolation.

Full test results in `~/Documents/ITMO/3 course/Functional Programming/fp-lab3/_build/default/test/_build/_tests/newton'.
Test Successful in 0.001s. 3 tests run.
Testing `io'.                      
This run has ID `41CZCZXW'.

  [OK]          input           0   parse ;.
  [OK]          input           1   parse ,.
  [OK]          input           2   parse tab.
  [OK]          input           3   parse space.
  [OK]          input           4   ignore header.
  [OK]          input           5   ignore empty.
  [OK]          output          0   format trims zeros.
  [OK]          output          1   format trims dot.
  [OK]          output          2   format rounding.

Full test results in `~/Documents/ITMO/3 course/Functional Programming/fp-lab3/_build/default/test/_build/_tests/io'.
Test Successful in 0.003s. 9 tests run.
Testing `stream'.                  
This run has ID `7T8XDJ0R'.

  [OK]          linear          0   no duplicate endpoint.
  [OK]          linear          1   process_point + finalize.
  [OK]          newton          0   requires window.
  [OK]          newton          1   outputs on window.

Full test results in `~/Documents/ITMO/3 course/Functional Programming/fp-lab3/_build/default/test/_build/_tests/stream'.
Test Successful in 0.001s. 4 tests run.
```

## Структура проекта

- bin/main.ml — CLI и запуск
- lib/input.ml — парсинг входного потока
- lib/linear.ml — линейная интерполяция
- lib/newton.ml — интерполяция Ньютона
- lib/interpolation.ml — потоковая обработка
- lib/output.ml — печать результата
- test/ — тесты (Alcotest)

## Примечания по реализации

- Ввод/вывод отделён от алгоритмов: парсинг — в `lib/input.ml`, печать — в `lib/output.ml`.
- Интерполяция Ньютона реализована без императивных циклов/таблиц (коэффициенты считаются через разделённые разности в функциональном стиле).
- Потоковый режим реализован через `Seq.t` (чтение построчно из stdin).

