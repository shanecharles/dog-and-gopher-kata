# Dog and Gopher (partial erlang)

This runs through the input file and determines whether the gopher escapes the dog.

## Running

Once the module is loaded you can run the simulation by calling:

    dog_gopher:run_simulation("some input file").


### OSx / Linux

    run.sh


### Windows

Open `werl`, change to the directory containing `dog_gopher.erl`.

    cd("some directory").
    c(dog_gopher).
    dog_gopher:run_simulation("../input.txt").

