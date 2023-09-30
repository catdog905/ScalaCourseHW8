# Disclaimer

В домашних заданиях вы можете менять сигнатуры методов/интерфейс, если на написано обратного. Если в задании нужно
реализовать определенную функцию, которая уже объявлена за вас, в таких случаях ее сигнатуру менять нельзя.

# Type Classes Hierarchy

Ваша задача - реализовать иерархию тайпклассов, данную в `factory.hierarchy.Typeclasses.scala`
для простого бинарного дерева, данного в `Tree.scala`. Написать тесты, проверяющие работу тайпклассов

# Работа с Eval

С помощью тайпкласса [Eval](https://typelevel.org/cats/datatypes/eval.html), напишите стекобезопасно:

1) Функцию, вычисляющую `n-ный` член последовательности Фибоначчи (0 - 0, 1 - 1, 2 - 1...):
   ```scala
      def fib(n: Int): Int = ???
   ```
2) Функцию foldRight:
    ```scala
      def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = ???
    ```

Написать тесты для проверки, что стек не взорвется

### Code Style:

Мы последовательно вводим список запрещенных механик, которыми нельзя пользоваться при написании кода, и рекомендаций по
code style. За нарушения мы оставляем за собой право **снижать оценку**.

* Переменные и функции должны иметь осмысленные названия;
* Тест классы именуются `<ClassName>Spec`, где `<ClassName>` - класс к которому пишутся тесты;
* Тест классы находятся в том же пакете, что и класс к которому пишутся тесты (например, класс `Fibonacci` находится в
  пакете `fibonacci` в директории `src/main/scala/fibonacci`, значит его тест класс `FibonacciSpec` должен быть в том же
  пакете в директории `src/test/scala/fibonacci`);
* Каждый тест должен быть в отдельном test suite;
* Использовать java коллекции запрещается (Используйте `Scala` коллекции);
* Использовать `mutable` коллекции запрещается;
* Использовать `var` запрещается;
* Использование `this` запрещается (используйте `self` если требуется);
* Использование `return` запрещается;
* Использование `System.exit` запрещается;
* Касты или проверки на типы с помощью методов из Java вроде `asInstanceOf` запрещаются;
* Использовать `throw` запрещено;
* Использование циклов запрещается (используйте `for comprehension`, `tailRec`, методы `Monad`, `fold`);
* Использование не безопасных вызовов разрешено только в тестах (например `.get` у `Option`);
* Использование взятия и освобождения примитивов синхронизации: semaphore, mutex - из разных потоков запрещено;
* Использование require для ошибок запрещается
