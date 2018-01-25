module type CollectorSpec = {type e; type r; let l: list(e); let f: e => r;};

module Collector = {
  module Make = (C: CollectorSpec) => {
    type e = C.e;
    type r = C.r;
    let collect = (~l=C.l, ~f=C.f, _) => {
      let result = Hashtbl.create(~random=true, List.length(l));
      List.iter(
        item => {
          let r = f(item);
          let newList = Hashtbl.mem(result, r) ? Hashtbl.find(result, r) : [];
          Hashtbl.replace(result, r, [item, ...newList]);
        },
        l
      );
      result;
    };
  };
};

module EvenNumbersCollectorSpec =
  Collector.Make(
    {
      type e = int;
      type r = bool;
      let l = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
      let f = n => n mod 2 === 0 ? true : false;
    }
  );

let m = EvenNumbersCollectorSpec.collect();

Js.log(Hashtbl.mem(m, true));

Js.log(Hashtbl.mem(m, false));

let trues = Hashtbl.find(m, true);

let falses = Hashtbl.find(m, false);

Js.log2("trues are:", Array.of_list(trues));

Js.log2("falses are:", Array.of_list(falses));

type animalType =
  | Dog
  | Cat
  | Horse;

type animal = {
  type_: animalType,
  name: string
};

let animals = [
  {type_: Dog, name: "Wuffles"},
  {type_: Dog, name: "Biscuits"},
  {type_: Cat, name: "KillerCat"}
];

module AnimalCollector =
  Collector.Make(
    {
      type e = animal;
      type r = animalType;
      let l = animals;
      let f = n => n.type_;
    }
  );

let m = AnimalCollector.collect();

Js.log(Array.of_list(Hashtbl.find(m, Dog)));

Js.log(Array.of_list(Hashtbl.find(m, Cat)));

Js.log(Hashtbl.mem(m, Horse));