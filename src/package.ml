open Cv

type package = {image: image}

let pack image = {image}

let unpack {image} = image
