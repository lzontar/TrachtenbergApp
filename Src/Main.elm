module Main exposing (Msg(..), main)

import Html exposing (..)
import Html.Attributes exposing (src, width)
import Html.Events exposing (onClick, onInput, on)
import Http
import Browser
import Json.Decode as D
import Html.Attributes exposing (..)

-->pravila racunanja, ki se pokazejo v aplikaciji
updateRules: Model-> Html Msg
updateRules mo=
  case mo.operation of
    "square" ->
      case mo.firstList of
        [_] -> Html.div [class "rulesDiv"][text "Just square it, it's quite simple..."]
        [5, _] -> Html.div [class "rulesDiv"][Html.div [][text "1. Square the units digit."], Html.div [][text "2. Add 25 to the units digit."]]
        [_, 5] -> Html.div [class "rulesDiv"][Html.div [][text "1. Multiply the tens digit by the next larger digit."]]
        [_, _] -> Html.div [class "rulesDiv"][Html.div [][text "1. Square the units digit."], Html.div [][text "2. Do an 'open cross-product', where you multiply the first and last digits then double the result."], Html.div [][text "1. Square the tens digit"]]
        [_, _ , _] ->Html.div [class "rulesDiv"][Html.div [][text "1. Ignore the hundreds digit and square the tens and unit digits using the method for squaring 2 digit numbers."], Html.div [][text "2. On the Hundreds and tens digits do another squaring 2 digit number but this time omit the first step of squaring the units digit."], Html.div [][text "1. Square the tens digit"]]
        _-> Html.div [][]
    "multiply" ->
      case (mo.firstFactor, mo.secondFactor) of
        (Just n, Just m) ->
          let
            bigger = if (n < m) then m else n
            smaller = if (n < m) then n else m
          in
          case smaller of
            0 -> Html.div [class "rulesDiv"][Html.div [][text "1. Zero times any number is always zero"]]
            1 -> Html.div [class "rulesDiv"][Html.div [][text "1. Copy each digit in the number."]]
            2 ->Html.div [class "rulesDiv"][Html.div [][text "1. Double the number."]]
            3 ->Html.div [class "rulesDiv"][Html.div [][text "1. First step: Subtract from 10 and double, and add 5 if the number is odd."], Html.div [][text "2. Middle steps: Subtract from 9, double and add 5 if the number is odd, and add “half” the neighbor."], Html.div [][text "3. Last step: Take “half” the left-hand digit of multiplicand and reduce by 2."]]
            4 ->Html.div [class "rulesDiv"][Html.div [][text "1. First step: Subtract from 10, and add 5 if the number is odd."], Html.div [][text "2. Middle steps: Subtract from 9 and add “half” the neighbor, plus 5 if the number is odd."], Html.div [][text "3. Last step: Take “half” the left-hand of the multiplicand and reduce by 1."]]
            5 -> Html.div [class "rulesDiv"][Html.div [][text "1. Use “half” the neighbor, plus 5 if the number is odd."]]
            6 -> Html.div [class "rulesDiv"][Html.div [][text "1. Add 5 to the number only if it is odd; Add “half” the neighbor."]]
            7 -> Html.div [class "rulesDiv"][Html.div [][text "1. Double the number and add 5 if the number is odd, and add “half” the neighbor."]]
            8 ->Html.div [class "rulesDiv"][Html.div [][text "1. First step: Subtract from 10 and double."], Html.div [][text "2. Middle steps: Subtract from 9 and double what you get, then add the neighbor."], Html.div [][text "3. Last step: Subtract two from the left-hand figure of the multiplicand."]]
            9 -> Html.div [class "rulesDiv"][Html.div [][text "1. First step: subtract from 10"], Html.div [][text "2. Middle steps: Subtract from 9 and add the neighbor."], Html.div [][text "3. Last step: Reduce the left hand digit of multiplicand by 1."]]
            10 -> Html.div [class "rulesDiv"][Html.div [][text "1. Use the neighbor."]]
            11 -> Html.div [class "rulesDiv"][Html.div [][text "1. Add the neighbor."]]
            12 -> Html.div [class "rulesDiv"][Html.div [][text "1. Double each number in turn and add its neighbor."]]
            _ -> Html.div [class "rulesDiv"][Html.div [][text "1. When multiplying by a multiplier of any length, put as many zeros before the multiplicand as there are digits in the multiplier."],Html.div [][text "2. We make n (number of digits in multiplier) auxiliary multiplications, where we multiply inner digit of multiplier with inner digit of multiplicand, outer digit of multiplier with outer digit of multiplicand and so on."], Html.div [][text "3. Move the inner digit of multiplicand in auxiliary multiplications to the left for one digit."]]
        (_, _) -> Html.div [][]
    _ -> Html.div [][]

-->iz danega stevila izvlecemo stevke in jih damo v seznam
intToList: Maybe Int -> (List Int)
intToList n =
    case n of
      Nothing ->
        []
      Just x ->
        if (x // 10 == 0) then
          [x]
        else
          [(modBy 10 x)] ++ (intToList (Just (x // 10)))

-->pretvorba seznama intov v string ---> firstList =[0,0,3,4,0,0] pretvori v "004300"
listToString: List Int -> String
listToString l =
  case l of
    [] -> ""
    (h::t) -> (listToString t) ++ (String.fromInt h)

-->zaradi lazje kalkulacije dodamo seznamu prvega stevila na zacetek in na konec (|dolzina drugega stevila| - 1) nicel
-->t.j. ce je prvo stevilo 45 in drugo 98 bosta seznama postala [0,5,4,0] in [9,8]
addZeros: Int -> List Int -> List Int
addZeros len list =
  if (len == 0) then
    list
  else
    addZeros (len - 1) ([0] ++ list ++ [0])


listMultiplications: Int -> Int -> List Int -> List Int -> List (((Int,Int),Int), String)
listMultiplications step nOfMultiplications list1 list2 =
  -->najprej se pomaknemo za stevilo korakov v levo
  if (step > 0) then
    case list1 of
      [] -> []
      (h::t) ->
        listMultiplications (step - 1) nOfMultiplications t list2
  -->potem pa opravimo |dolzina drugega stevila| pomoznih mnozenj in vsako shranimo v seznam
  else
    if (nOfMultiplications <= 0) then
      []
    else
      case (list1, list2) of
        ((h1::t1),(h2::t2)) ->
            (((h1, h2), h1*h2), " * ") :: (listMultiplications step nOfMultiplications t1 t2)
        (_,_)-> []

-->vsota pomoznih mnozenj
sumOfMultiplications: List (((Int, Int), Int),String) -> Int
sumOfMultiplications list =
  case list of
    [] -> 0
    (h::t) -> (Tuple.second (Tuple.first h)) + (sumOfMultiplications t)

-->pokazemo naslednji korak pri normalnem množenju
showNextStepMultiplying:Model->Model
showNextStepMultiplying m =
    let
      multiplications = (listMultiplications (m.step - 1) (List.length m.secondList) m.firstList m.secondList)
      multiplicationsSum = sumOfMultiplications multiplications + m.carry
      nextStep = m.result ++ [modBy 10 multiplicationsSum]
    in
    {m | step = m.step + 1,prevCarry = m.carry ,carry = multiplicationsSum // 10, result = nextStep, stepCalculations = multiplications, isFinished = m.step - 1 >= (List.length m.firstList) - (List.length m.secondList - 1) - 1}

-->pokazemo naslednji korak pri kvadriranju
showNextStepSquare: Model -> Model
showNextStepSquare m =
  case (List.length m.firstList) of

    1 ->
      case m.firstFactor of
        Nothing -> m
        Just n ->
          {m | result = intToList (Just (n * n)), step = m.step + 1, isFinished = True, stepCalculations = [(((n, n), n * n), " * ")] }
    2 ->
      case m.firstList of
        [] -> m
        -->Prvi clen v m.firstList so enice, drugi clen pa desetice
        [5,n] ->
          -->pogledamo kateri korak je
          case m.step of
            1->
              {m | step = m.step + 1, result = intToList (Just (25)), stepCalculations = [(((5, 5), 25), " * ")]}
            2->
              {m | step = m.step + 1, result = m.result ++ (intToList (Just (n * (n + 1)))), stepCalculations = [(((n, 1), n + 1), " + "),(((n, n + 1), n * (n + 1)), " * ")], isFinished = True}
            _ -> {m | step = m.step + 1}
        [n,5] ->
          -->pogledamo kateri korak je
          case m.step of
            1->
              {m | step = m.step + 1, result = intToList (Just (n * n)) ++ (if (n * n < 10) then [0] else []), stepCalculations = [(((n, n), n * n), " * ")]}
            2->
              {m | step = m.step + 1, result = m.result ++ (intToList (Just (n + 25))), stepCalculations = [(((n, 25), n + 25), " + ")], isFinished = True}
            _ -> {m | step = m.step + 1}
        [x, y] ->
          case m.step of
            1 ->
              {m | step = m.step + 1, result = intToList (Just (modBy 10 (x*x))), carry = x*x // 10, stepCalculations = [(((x, x), x * x), " * ")]}
            2 ->
              let
                multiplication = m.carry + x * y * 2
              in
              {m | step = m.step + 1, result = m.result ++ intToList (Just (modBy 10 multiplication)), carry = multiplication // 10, stepCalculations = [(((x, y), x * y), " * "), (((x*y,2),x*y*2), " * ")], prevCarry = m.carry}
            3 ->
              let
                multiplication = m.carry + y * y
              in
              {m | step = m.step + 1, result = m.result ++ intToList (Just (modBy 10 multiplication)), carry = multiplication // 10, stepCalculations = [(((y, y), y * y), " * ")], prevCarry = m.carry}
            4 ->
              {m | step = m.step + 1, result = m.result ++ (if (m.carry /= 0) then [m.carry] else []), isFinished = True}
            _ -> m
        _ ->
          m
    3->
      case m.firstList of
      -->x enice, y desetice, z stotice
          [x,y,z] ->
            case m.step of
              1 ->
                {m | step = m.step + 1, result = intToList (Just (modBy 10 (x * x))), carry = (x * x) // 10, stepCalculations = [(((x, x), x * x), " * ")]}
              2 ->
                let
                  multiplication = x * y * 2 + m.carry
                in
                {m | step = m.step + 1, result = m.result ++ intToList (Just (modBy 10 multiplication)), carry = multiplication // 10, stepCalculations = [(((x, y), x * y), " * "),(((x * y, 2), x * y * 2), " * "),(((x * y * 2, m.carry), multiplication), " + ")], prevCarry = m.carry}
              3 ->
                {m | step = m.step + 1, stepCalculations = [(((y, y), y * y), " * "), (((y * y, m.carry), y * y + m.carry), " + ")], prevCarry = m.carry, carry = y * y + m.carry}
              4 ->
                let
                  multiplication = x * z * 2 + m.carry
                in
                {m | step = m.step + 1, result = m.result ++ intToList (Just (modBy 10 multiplication)), carry = multiplication // 10, stepCalculations = [(((x, z), x * z), " * "), (((x * z, 2), x * z * 2), " * "), (((x * z * 2, m.carry), x * z * 2 + m.carry), " + ")], prevCarry = m.carry}
              5 ->
                let
                  multiplication = y * z * 2 + m.carry
                in
                {m | step = m.step + 1, result = m.result ++ intToList (Just (modBy 10 multiplication)), carry = multiplication // 10, stepCalculations = [(((y, z), y * z), " * "), (((y * z, 2), y * z * 2), " * "), (((y * z * 2, m.carry), y * z * 2 + m.carry), " + ")], prevCarry = m.carry}
              6 ->
                  let
                    multiplication = z * z + m.carry
                  in
                {m | step = m.step + 1, result = m.result ++ intToList (Just (multiplication)), carry = multiplication // 10, stepCalculations = [(((z, z), z * z), " * "), (((z * z, m.carry), z * z + m.carry), " + ")], prevCarry = m.carry}
              _ -> m
          _ -> m
    _->
      m

doubleTheNumber: Model -> Model
doubleTheNumber m =
    case (List.reverse m.result) of
      h::t->
        let
          auxCalculation = h * 2
          wantedResult = (List.reverse t) ++ [(modBy 10 (h * 2))]
          wantedCarry = m.carry + (h * 2) // 10
        in
        {m | result = wantedResult, carry = wantedCarry, stepCalculations = m.stepCalculations ++[(((h,2),h*2), " * ")]}
      []->m

substractFrom: Model-> Int -> Int-> Model
substractFrom m minuend num =
  case (List.reverse m.result) of
    h::t ->
      let
        wantedResult = (List.reverse t) ++ [(minuend - num)]
      in
      {m | result = wantedResult, stepCalculations = m.stepCalculations ++ [(((minuend,num), minuend - num), " - ")]}
    []->m

-->elementu pristeje 5, ce je le ta lih (num % 2 == 1)
addFiveIfOdd: Model->Int->Model
addFiveIfOdd m num=
  case (List.reverse m.result) of
    h::t ->
      if (modBy 2 num == 1) then
        let
          wantedResult = (List.reverse t) ++ [(modBy 10 (h + 5))]
          wantedCarry = m.carry + ((h + 5) // 10)
        in
        {m | carry = wantedCarry, result = wantedResult, stepCalculations = m.stepCalculations ++ [(((h,5), h + 5), " + ")]}
      else
        m
    [] -> m
-->elementu dodamo (neighbour / divisor)
addNeigbour: Model-> Int->Int ->Model
addNeigbour m divisor neighbour =
  case (List.reverse m.result) of
    hRes::tRes->
      let
        wantedResult = (List.reverse tRes) ++ [(modBy 10 (hRes + neighbour//divisor))]
        wantedCarry = m.carry + ((hRes + neighbour//divisor) // 10)
      in
      {m | carry = wantedCarry, result = wantedResult, stepCalculations = m.stepCalculations ++ [(((hRes + 10 * m.prevCarry,neighbour//2), hRes + 10 * m.prevCarry + neighbour//2), " + ")]}
    [] -> m



-->poisce element na ix-tem mestu v listu
intInList:Int-> List Int -> Int
intInList ix l =
  if (ix - 1 > 0) then
    case l of
      [] -> 0
      h::t-> intInList (ix - 1) t
  else
    case l of
      h::t-> h
      []->0

reduceLeftHandDigit:Model -> Int -> Int -> Int -> Model
reduceLeftHandDigit m neighbour reduceBy divisor=
  case (List.reverse m.result) of
    h::t ->
      let
        auxCalc = neighbour // divisor - reduceBy + m.prevCarry
        wantedResult = (List.reverse t) ++ (if (auxCalc > 0) then [auxCalc] else [])
      in
      {m | step = m.step + 1, result = wantedResult, stepCalculations = [(((neighbour//divisor,m.prevCarry), neighbour // divisor + m.prevCarry), " + "),(((neighbour // divisor + m.prevCarry,reduceBy), neighbour // divisor + m.prevCarry - reduceBy), " - ")]}
    [] -> m

addCarry: Model-> Model
addCarry m =
  case (List.reverse m.result) of
    h::t ->
      let
        wantedResult = (List.reverse t) ++ [(modBy 10 (h + m.prevCarry))]
        wantedCarry = m.carry + (h + m.prevCarry) // 10
      in
      {m | carry = wantedCarry, result = wantedResult, stepCalculations = m.stepCalculations ++ (if (m.prevCarry /= 0) then [(((h + m.carry * 10,m.prevCarry), h + m.carry * 10 + m.prevCarry), " + ")] else [])}
    []-> m

showNextStepMultiplyingSmallerNs: Model -> Model
showNextStepMultiplyingSmallerNs m =
  case (if (Maybe.withDefault 0 m.firstFactor < Maybe.withDefault 0 m.secondFactor) then ((m.firstFactor, m.firstList), (m.secondFactor, m.secondList)) else ((m.secondFactor, m.secondList), (m.firstFactor, m.firstList))) of
    ((Just smaller, lSmaller), (Just bigger, lBigger)) ->
      case smaller of
        0->
          {m | step = m.step + 1, result = [0], isFinished = True, stepCalculations = [(((bigger,0), 0), " * ")]}
        1->
          {m | step = m.step + 1, result = intToList (Just bigger), isFinished = True, stepCalculations = [(((bigger,1), bigger), " * ")]}
        2->
            let
              numAtIx = intInList m.step lBigger
              doubled = doubleTheNumber {m | result = List.reverse (numAtIx :: List.reverse m.result),stepCalculations = [], prevCarry = m.carry, carry = 0}
              n = addCarry doubled
            in
            {n | step = n.step + 1,isFinished = (n.step - 1 == List.length lBigger && m.carry == 0) || (n.step - 1 > List.length lBigger)}
        3->
          if (m.step == 1) then
            let
              numAtIx = intInList (m.step) lBigger
              substractedM = substractFrom  {m | result = List.reverse (0 :: List.reverse m.result),stepCalculations = [], prevCarry = m.carry, carry = 0} 10 numAtIx
              doubledM = doubleTheNumber substractedM
              addedFiveIfOddM = addFiveIfOdd doubledM numAtIx
              n = addedFiveIfOddM
            in
            {n | step = n.step + 1}

          else if (m.step - 1 >= List.length lBigger) then
            let
              neighbour = intInList (m.step - 1) lBigger
              numAtIx = intInList (m.step) lBigger
              reducedM = reduceLeftHandDigit {m | result = List.reverse (0 :: List.reverse m.result),stepCalculations = [], prevCarry = m.carry, carry = 0} neighbour 2 2
              n = reducedM
            in
            {n | step = n.step + 1, isFinished = True}
          else
            let
              neighbour = intInList (m.step - 1) lBigger
              numAtIx = intInList (m.step) lBigger
              substractedM = substractFrom  {m | result = List.reverse (0 :: List.reverse m.result),stepCalculations = [], prevCarry = m.carry, carry = 0} 9 numAtIx
              doubledM = doubleTheNumber substractedM
              addedFiveIfOddM = addFiveIfOdd doubledM numAtIx
              addedNeighbourM = addNeigbour addedFiveIfOddM 2 neighbour
              addedCarryM = addCarry addedNeighbourM
              n = addedCarryM
            in
            {n | step = n.step + 1,isFinished = (n.step - 1 == List.length lBigger && m.carry == 0) || (n.step - 1 > List.length lBigger)}
        4->
          if (m.step == 1) then
            let
              neighbour = intInList (m.step - 1) lBigger
              numAtIx = intInList (m.step) lBigger
              substractedM = substractFrom  {m | result = List.reverse (0 :: List.reverse m.result),stepCalculations = [], prevCarry = m.carry, carry = 0} 10 numAtIx
              addedFiveIfOddM = addFiveIfOdd substractedM numAtIx
              n = addedFiveIfOddM
            in
            {n | step = n.step + 1}
          else if (m.step - 1 >= List.length lBigger) then
            let
              neighbour = intInList (m.step - 1) lBigger
              numAtIx = intInList (m.step) lBigger
              reducedM = reduceLeftHandDigit {m | result = List.reverse (0 :: List.reverse m.result),stepCalculations = [], prevCarry = m.carry, carry = 0} neighbour 1 2
              n = reducedM
            in
            {n | step = n.step + 1, isFinished = True}
          else
            let
              numAtIx = intInList (m.step) lBigger
              neighbour = intInList (m.step - 1) lBigger
              substractedM = substractFrom  {m | result = List.reverse (0 :: List.reverse m.result),stepCalculations = [], prevCarry = m.carry, carry = 0} 9 numAtIx
              addedNeighbourM = addNeigbour substractedM 2 neighbour
              addedFiveIfOddM = addFiveIfOdd addedNeighbourM numAtIx
              addedCarryM = addCarry addedFiveIfOddM
              n = addedCarryM
            in
            {n | step = n.step + 1, isFinished = (n.step - 1 == List.length lBigger && m.carry == 0) || (n.step - 1 > List.length lBigger)}
        5->
          let
            numAtIx = intInList (m.step) lBigger
            neighbour = if (m.step == 1) then 0 else intInList (m.step - 1) lBigger
            addedNeighbourM = addNeigbour {m | result = List.reverse (0 :: List.reverse m.result),stepCalculations = [], prevCarry = m.carry, carry = 0} 2 neighbour
            addedFiveIfOddM = addFiveIfOdd addedNeighbourM numAtIx
            addedCarryM = addCarry addedFiveIfOddM
            n = addedCarryM
          in
          {n | step = n.step + 1, isFinished = (n.step - 1 == List.length lBigger && m.carry == 0) || (n.step - 1 > List.length lBigger)}
        6->
          let
            numAtIx = intInList (m.step) lBigger
            neighbour = if (m.step == 1) then 0 else intInList (m.step - 1) lBigger
            addedFiveIfOddM = addFiveIfOdd {m | result = List.reverse (numAtIx :: List.reverse m.result),stepCalculations = [], prevCarry = m.carry, carry = 0} numAtIx
            addedNeighbourM = addNeigbour addedFiveIfOddM 2 neighbour
            addedCarryM = addCarry addedNeighbourM
            n = addedCarryM
          in
          {n | step = n.step + 1, isFinished = (n.step - 1 == List.length lBigger && m.carry == 0) || (n.step - 1 > List.length lBigger)}
        7->
          let
            numAtIx = intInList (m.step) lBigger
            neighbour = if (m.step == 1) then 0 else intInList (m.step - 1) lBigger
            doubledM = doubleTheNumber {m | result = List.reverse (numAtIx :: List.reverse m.result),stepCalculations = [], prevCarry = m.carry, carry = 0}
            addedFiveIfOddM = addFiveIfOdd doubledM numAtIx
            addedNeighbourM = addNeigbour addedFiveIfOddM 2 neighbour
            addedCarryM = addCarry addedNeighbourM
            n = addedCarryM
          in
          {n | step = n.step + 1, isFinished = (n.step - 1 == List.length lBigger && m.carry == 0) || (n.step - 1 > List.length lBigger)}
        8->
          if (m.step == 1) then
            let
              neighbour = intInList (m.step - 1) lBigger
              numAtIx = intInList (m.step) lBigger
              substractedM = substractFrom  {m | result = List.reverse (0 :: List.reverse m.result),stepCalculations = [], prevCarry = m.carry, carry = 0} 10 numAtIx
              doubledM = doubleTheNumber substractedM
              n = doubledM
            in
            {n | step = n.step + 1}
          else if (m.step - 1 >= List.length lBigger) then
            let
              neighbour = intInList (m.step - 1) lBigger
              numAtIx = intInList (m.step) lBigger
              reducedM = reduceLeftHandDigit {m | result = List.reverse (0 :: List.reverse m.result),stepCalculations = [], prevCarry = m.carry, carry = 0} neighbour 1 2
              n = reducedM
            in
            {n | step = n.step + 1, isFinished = True}
          else
            let
              numAtIx = intInList (m.step) lBigger
              neighbour = intInList (m.step - 1) lBigger
              substractedM = substractFrom  {m | result = List.reverse (0 :: List.reverse m.result),stepCalculations = [], prevCarry = m.carry, carry = 0} 9 numAtIx
              doubledM = doubleTheNumber substractedM
              addedNeighbourM = addNeigbour doubledM 1 neighbour
              addedCarryM = addCarry addedNeighbourM
              n = addedCarryM
            in
            {n | step = n.step + 1}
        9->
          if (m.step == 1) then
            let
              neighbour = intInList (m.step - 1) lBigger
              numAtIx = intInList (m.step) lBigger
              substractedM = substractFrom  {m | result = List.reverse (0 :: List.reverse m.result),stepCalculations = [], prevCarry = m.carry, carry = 0} 10 numAtIx
              n = substractedM
            in
            {n | step = n.step + 1}
          else if (m.step - 1 >= List.length lBigger) then
            let
              neighbour = intInList (m.step - 1) lBigger
              numAtIx = intInList (m.step) lBigger
              reducedM = reduceLeftHandDigit {m | result = List.reverse (0 :: List.reverse m.result),stepCalculations = [], prevCarry = m.carry, carry = 0} neighbour 1 1
              n = reducedM
            in
            {n | step = n.step + 1, isFinished = True}
          else
            let
              numAtIx = intInList (m.step) lBigger
              neighbour = intInList (m.step - 1) lBigger
              substractedM = substractFrom  {m | result = List.reverse (0 :: List.reverse m.result),stepCalculations = [], prevCarry = m.carry, carry = 0} 9 numAtIx
              addedNeighbourM = addNeigbour substractedM 1 neighbour
              addedCarryM = addCarry addedNeighbourM
              n = addedCarryM
            in
            {n | step = n.step + 1}
        10->
          {m | step = m.step + 1, isFinished = True, result = intToList (Just (bigger * 10)), stepCalculations = [(((bigger,10), bigger * 10), " * ")]}
        11->
          let
            numAtIx = intInList (m.step) lBigger
            neighbour = if (m.step == 1) then 0 else intInList (m.step - 1) lBigger
            addedNeighbourM = addNeigbour {m | result = List.reverse (numAtIx :: List.reverse m.result),stepCalculations = [], prevCarry = m.carry, carry = 0} 1 neighbour
            addedCarryM = addCarry addedNeighbourM
            n = addedCarryM
          in
          {n | step = n.step + 1, isFinished = (n.step - 1 >= List.length lBigger)}
        12->
          let
            numAtIx = intInList (m.step) lBigger
            neighbour = if (m.step == 1) then 0 else intInList (m.step - 1) lBigger
            doubledM = doubleTheNumber {m | result = List.reverse (numAtIx :: List.reverse m.result),stepCalculations = [], prevCarry = m.carry, carry = 0}
            addedNeighbourM = addNeigbour doubledM 1 neighbour
            addedCarryM = addCarry addedNeighbourM
            n = addedCarryM
          in
          {n | step = n.step + 1, isFinished = (n.step - 1 == List.length lBigger && m.carry == 0) || (n.step - 1 > List.length lBigger)}
        _->m
    _-> m

-->pomozne funkcije, za mnozenje z manjsimi stevili(x <= 12)
-->seznam pomoznih mnozenj
showAuxCalculations: Model -> List (Html Msg)
showAuxCalculations m =
  case m.stepCalculations of
    (((a, b), res), op) :: t ->
      (Html.div [][text ((String.fromInt a) ++ op ++ (String.fromInt b) ++ " = " ++ (String.fromInt res))]) :: (showAuxCalculations {m | stepCalculations = t})
    _ -> []

-->pretvorba seznama stevk rezultata v string
resultToString: List Int -> String
resultToString l =
  case l of
    h::t ->
      case t of
        h1::t1 ->
          (resultToString t)++ (String.fromInt h)
        [] -> if (h == 0) then "" else String.fromInt h
    [] -> ""
type alias Model =
    {
    -->zaporedna številka koraka, ki ga izvajamo
      step : Int ,
    -->seznam pomoznih racunov trenutnega koraka
      stepCalculations:List (((Int, Int), Int), String),
    -->shranjuje trenutni rezultat v nasprotnem vrstnem redu
      result: List Int,
    -->prvi faktor
      firstFactor : Maybe Int,
    -->drugi faktor
      secondFactor : Maybe Int,
    -->števke prvega faktorja v nasprotnem vrstnem redu (59 = [9,5] + ničle)
      firstList : List Int,
    -->števke drugega faktorja
      secondList : List Int,
    -->prenos koraka
      carry: Int,
    -->prenos prejšnjega koraka
      prevCarry: Int,
    -->naključno dejstvo iz NumberAPI
      currFunFact : String,
    -->indikator ali je izračun končan
      isFinished : Bool,
    --> String z informacijo o operaciji, ki jo uporabnik izvaja
      operation : String,
      -->trenutni odgovor
      currAnswer : Maybe Int,
    -->stevilo napacnih odgovorov
      wrongAnswers : Int,
    --> Html Msg , ki vsebuje pravila mnozenja trenutnih stevil
      rules : Html Msg
    }
type alias Style =
    ( String, String )

type Msg =
    GetRandomFact |
    NewFunFact (Result Http.Error String) |
    InputFirst String |
    InputSecond String |
    NextStep |
    ChangeOperation String |
    InputAnswer String |
    CheckAnswer

update:Msg->Model->(Model,Cmd Msg)
update msg m =
    case msg of
      GetRandomFact ->
        (m, Http.get {expect = Http.expectString NewFunFact , url = "http://numbersapi.com/random"})
      NewFunFact (Ok newFact) ->
        ({m | currFunFact = newFact},Cmd.none)
      NewFunFact (Err _) ->
        (m,Cmd.none)
      InputFirst text ->
        ({m | firstFactor = String.toInt text, firstList = intToList (String.toInt text), secondList = if (m.firstFactor /= Nothing && Maybe.withDefault 0 m.firstFactor <= 12) then (intToList (String.toInt text)) else List.reverse (intToList (String.toInt text)) , step = 0, carry = 0, result = [], stepCalculations = [], isFinished = False, prevCarry = 0, wrongAnswers=0}, Cmd.none)
      InputSecond text ->
        ({m | secondFactor = String.toInt text, firstList = intToList (m.firstFactor), secondList = if (m.firstFactor /= Nothing && Maybe.withDefault 0 m.firstFactor <= 12) then (intToList (String.toInt text)) else List.reverse (intToList (String.toInt text)) , step = 0, carry = 0, result = [], isFinished = False, prevCarry = 0, wrongAnswers=0, stepCalculations = []}, Cmd.none)
      NextStep ->
        -->za lazjo kalkulacijo dodamo nicle spredaj in zadaj prvega faktorja, ce ne gre za kvadriranje
        case (m.step, m.operation) of
          (0, "multiply") -> update CheckAnswer {m | step = m.step + 1, rules = updateRules m, firstList = if (Maybe.withDefault 0 m.firstFactor > 12 && Maybe.withDefault 0 m.secondFactor > 12) then addZeros ((List.length m.secondList) - 1) (intToList m.firstFactor) else m.firstList}
          (0, "square") -> update CheckAnswer {m | step = m.step + 1, rules = updateRules m}
          (_,"multiply") -> update CheckAnswer (if (Maybe.withDefault 0 m.firstFactor > 12 && Maybe.withDefault 0 m.secondFactor > 12) then showNextStepMultiplying m else showNextStepMultiplyingSmallerNs m)
          (_, "square") -> update CheckAnswer (showNextStepSquare m)
          (_,_)-> (m, Cmd.none)
      ChangeOperation op->
        ({m | operation = op, step = 0, stepCalculations = [], result = [], carry = 0, prevCarry = 0, isFinished = False, firstList= intToList m.firstFactor, rules = updateRules m}, Cmd.none)
      InputAnswer text ->
        ({m | currAnswer = String.toInt text}, Cmd.none)
      CheckAnswer ->
        case m.currAnswer of
          Just n ->
            case (List.reverse m.result) of
              (h::t) ->
                ({m | wrongAnswers = m.wrongAnswers + (if ((h + m.carry * 10) == n) then 0 else 1)}, Cmd.none)
              [] -> (m, Cmd.none)
          Nothing ->
            ({m | wrongAnswers = m.wrongAnswers + 1}, Cmd.none)

view:Model->Html Msg
view m =
  Html.div [class "body"][
    nav[][text "Array starts with 0"],
    Html.div [class "inputDiv"][input [onInput InputFirst][],
      if(m.operation /= "square") then input [onInput InputSecond][] else text "",
      select [onInput ChangeOperation][option [value "multiply"][text "Multiply"], option [value "square"][text "Square"]]],
    m.rules,
    Html.div [class "mainDiv"][
      Html.div [class "calculationDiv"][if (m.firstFactor /= Nothing && m.secondFactor /= Nothing && m.operation == "multiply") then
          text ((listToString  m.firstList) ++ " * " ++ (String.fromInt (Maybe.withDefault 0 m.secondFactor)))
          else if (m.firstFactor /= Nothing && m.operation == "square") then
            text (String.fromInt (Maybe.withDefault 0 m.firstFactor))
          else
            text "Please enter both factors, that you would like to multiply using Trachtenberg method.",
          if (m.firstFactor /= Nothing && m.operation == "square") then (sup [][text "2"]) else (text ""), if(m.result /= []) then Html.text (" = " ++ (if (m.isFinished == True) then (resultToString m.result) else listToString m.result)) else text ""],
      Html.div [class "auxCalcDiv"]([if (m.prevCarry /= 0) then text (String.fromInt m.prevCarry) else text ""] ++ (showAuxCalculations m)),
      if (m.isFinished == False) then Html.div [class "nextStepDiv"][
        if (m.step >= 1) then Html.div [class "answerDiv"][text "What is the result of auxiliary calculations?", if (m.isFinished == False && m.firstFactor /= Nothing && ((m.secondFactor /= Nothing) || (m.operation == "square"))) then input [onInput InputAnswer][] else text ""] else text "",
        if (m.firstFactor /= Nothing && (m.secondFactor /= Nothing || m.operation == "square") && m.isFinished == False) then button [onClick NextStep][span [][if (m.step == 0) then text "Begin multiplying" else text "Next step"]] else text ""
        ] else text ""
      ],
      Html.div [class "finishedDiv", if (m.isFinished) then style "display" "block" else style "display" "none"][
        Html.img [src "https://www.freeiconspng.com/uploads/close-button-png-27.png", width 50, height 50][],
        Html.div [][text "Good Job! You've made it to the end."],
        Html.div [][text ("Number of wrong answers: " ++ (String.fromInt m.wrongAnswers))],
        Html.div [][if (m.wrongAnswers == 0) then text "You really nailed it." else text "You can do better than that, right?"],
        Html.div [][if(m.wrongAnswers == 0) then text "Reward: Infinite number of random fun facts about numbers." else text "If you get all the answers right, you will be rewarded!",
          if (m.wrongAnswers == 0) then Html.div [class "funFactDiv"][text (m.currFunFact),
            button [onClick GetRandomFact][span [][text "Fun Fact"]]] else text ""
        ]
      ]
    ]




model:Model
model =
  {

   step = 0,
   stepCalculations = [],
   result = [],
   currFunFact = "",
   firstFactor = Nothing,
   secondFactor = Nothing,
   firstList = [],
   secondList = [],
   carry = 0,
   prevCarry = 0,
   isFinished = False,
   operation = "multiply",
   wrongAnswers = 0,
   currAnswer = Nothing,
   rules = Html.div [][]
 }

init: () -> (Model, Cmd Msg)
init _ =
    (model, Cmd.none)
main =
    Browser.element {init=init, view=view, update=update, subscriptions = \_ -> Sub.none}
