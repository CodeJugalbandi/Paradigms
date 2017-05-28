import java.util.function.*;
import java.util.stream.*;
import static java.util.stream.Collectors.*;
import java.util.*;

class Luhn {
  static boolean test(String creditCard) {
    String ccReversed = new StringBuilder(creditCard).reverse().toString();
    Map<Boolean, List<Integer>> oddEvens = IntStream.range(1, creditCard.length() + 1)
      .mapToObj(i -> Arrays.asList(Integer.parseInt(String.valueOf(ccReversed.charAt(i-1))), i))
      .collect(partitioningBy(x -> x.get(1) % 2 == 0, mapping(x -> x.get(0), toList())));

    Integer s1 = oddEvens.get(false).stream()
      .reduce(0, (x,y) -> x + y);
    Integer s2 = oddEvens.get(true).stream()
      .map(x -> 2 * x)
      .map(x -> (x > 9) ? (x % 10) + 1 : x)
      .reduce(0, (x,y) -> x + y);
    return String.valueOf(s1 + s2).endsWith("0");
  }
  
  public static void main(String[] args) {
    List<String> creditCards = Arrays.asList("2621195162335", "49927398716", "1234567812345670", "4485284720134093","49927398717", "1234567812345678");
    creditCards.stream().filter(Luhn::test)
      .forEach(System.out::println);
  }
}