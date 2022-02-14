# obsproc
Installation instructions for obsproc code adhere to NCO's working practices, so are slightly modified from industry practices. obsproc code will be built and installed in the same space as the local clone.

Clone the obsproc repository:
```bash
git clone https://github.com/noaa-emc/obsproc
```

Then
```bash
ls
cd obsproc
branch -a
git checkout feature/branchname
ls
```

build.sh default to building for WCOSS2, but it also supports Hera and Orion by setting `INSTALL_TARGET=hera/orion/wcoss2`

You may do the following instructions from develop or any feature branch.

You will see a `build.sh` script. You will run this to build and install the obsproc code in your local clone space:
```bash
./build.sh
```

You will have a new `exec/` directory in your clone space.

Installation is complete.
