from dataclasses import dataclass


@dataclass
class Location:
    startline: int
    startcol: int
    endline: int
    endcol: int

    # TODO: some merge logic to merge two locations
