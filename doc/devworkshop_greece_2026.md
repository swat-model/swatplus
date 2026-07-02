# SWAT+ Source Code Exercises


## SWAT+ Conference 2026, Thessaloniki, Greece

[Workshop Agenda](Swatplus_workshop_agenda.md)

---

## Contents

- [1 How to use this guide](#1-how-to-use-this-guide)
- [2 The GitHub Codespaces environment](#2-the-github-codespaces-environment)
  - [2.1 What a Codespace gives you](#21-what-a-codespace-gives-you)
  - [2.2 Create a fork in Github](#22-create-a-fork-in-github)
  - [2.3 Launching the Codespace](#23-launching-the-codespace)
  - [2.4 Before the session: confirm the toolchain](#24-before-the-session-confirm-the-toolchain)
  - [2.5 First build](#25-first-build)
  - [2.6 First run and first debug](#26-first-run-and-first-debug)
  - [2.7 Orientation: the source layout and the program shape](#27-orientation-the-source-layout-and-the-program-shape)
- [3 Exercise 1: Expose the Hargreaves exponent](#3-exercise-1-expose-the-hargreaves-exponent)
  - [3.1 Background](#31-background)
  - [3.2 Guided discovery](#32-guided-discovery)
  - [3.3 References](#33-references)
  - [3.4 The exercise](#34-the-exercise)
  - [3.5 Rebuild and verify (in Codespaces)](#35-rebuild-and-verify-in-codespaces)
  - [3.6 Discussion prompts](#36-discussion-prompts)
  - [3.7 Commit and Push Your Changes](#37-commit-and-push-your-changes)
  - [3.8 Shutdown Codespaces](#38-shutdown-codespaces)
- [4 Exercise 2: Add a constant daily PET method](#4-exercise-2-add-a-constant-daily-pet-method)
  - [4.1 Motivation and orientation](#41-motivation-and-orientation)
  - [4.2 The five steps](#42-the-five-steps)
  - [4.3 Activate and verify](#43-activate-and-verify)
  - [4.4 Discussion prompts](#44-discussion-prompts)
  - [4.5 Commit and Push Your Changes](#45-commit-and-push-your-changes)
  - [4.6 Shutdown Codespaces](#46-shutdown-codespaces)
- [5 Quick reference](#5-quick-reference)
  - [5.1 File and line locations (commit 5ccf6f0)](#51-file-and-line-locations-commit-5ccf6f0)
  - [5.2 Codespaces command cheat-sheet](#52-codespaces-command-cheat-sheet)
  - [5.3 Switching compilers (gfortran and ifx)](#53-switching-compilers-gfortran-and-ifx)
  - [5.4 Re-verifying after a source update](#54-re-verifying-after-a-source-update)

---

## 1 How to use this guide

This guide covers **two** source-code exercises, run entirely inside **GitHub Codespaces**:

1. **Expose the Hargreaves exponent.** Modify existing code. A one-parameter change that
   touches five files and teaches the full "add a basin parameter" pattern.
2. **Add a constant daily PET method.** Add new code. A new source file plus a dispatcher
   `case`, teaching the "add a method to a `select case`" pattern.

The guide is **self-paced**: there is no minute-by-minute clock. Each exercise is a complete
chapter (background, guided discovery, full solution, verification, and discussion prompts)
that a learner can work through alone or that you can drive live. Solutions are given in full;
everything marked *Teacher note* is for you, not the participant.

> [!WARNING]
> **Use gfortran for these exercises.** The container also includes Intel `ifx`, but the
> `ifx` build was not debuggable here: with the stock `gdb` breakpoints would not bind, and
> pointing the launch configuration at Intel's own `gdb-oneapi` did not fix it either. With
> `gfortran` and the bundled `gdb`, breakpoints, stepping, and variable inspection all work.
> Every build, run, and debug step in this guide uses `gfortran`. `ifx` stays available for
> an Intel-optimised build, but do the hands-on work with `gfortran`.

> [!NOTE]
> **How the build works in Codespaces.** The build is **CMake + gfortran** and debugging is
> **gdb driven from the editor**. Three things are handled for you:
> 
> - **`main.f90` is generated.** CMake's `configure_file` produces `src/main.f90`
>   automatically at configure time (`CMakeLists.txt:131–138`).
> - **New source files are auto-collected.** `.f90` files are gathered by `file(GLOB ...)`
>   (`CMakeLists.txt:150`); you only re-run *configure* when you add one (see Exercise 2).
> - **The toolchain is pre-installed.** The dev container ships `gfortran` (and `ifx`),
>   CMake, and `gdb` ready to use.

---

## 2 The GitHub Codespaces environment

### 2.1 What a Codespace gives you

A Codespace is a Linux container (Ubuntu Jammy) defined by `.devcontainer/`. The dev
container installs:

- **gfortran** (the compiler used throughout this guide) and the **Intel oneAPI Fortran
  compiler** (`ifx`),
- **CMake** and **gdb**,
- the VS Code **CMake Tools**, **Modern Fortran**, and **C/C++** extensions (the last
  provides the gdb debug bridge).

You can open the Codespace in the **browser** or attach **VS Code Desktop**; both behave
identically. Keyboard shortcuts (`Ctrl+Shift+F` for search, `F5` to debug, `F7` to build)
work in the browser too.

### 2.2 Create a fork in Github

> [!NOTE]
> **What is a fork?** A fork is your own personal copy of a repository on GitHub. It lives
> under your GitHub account and is fully independent: you can make commits, push branches,
> and open pull requests without affecting the original repository. Forks are the standard
> way to contribute to an open-source project — you work in your fork and then propose your
> changes back to the upstream repository via a pull request.

If you haven't already created your fork, create a Github fork off of the swatplus main
repository. Here are the steps:

1. Go to the Github swatplus main repository 
   [https://github.com/swatplus/swatplus](https://github.com/swatplus/swatplus).
2. Click on **Fork** and select the owner (which is yourself) and a name for your fork,
   then select **Create fork**.

### 2.3 Launching the Codespace

1. On your forked repository page, click the green **<> Code** button → **Top Left
   Hamburger Icon** → **codespaces**.
2. Select **New Codespace** 
3. For the repository select your forked repository then for the branch, select **greese_dev**.
4. Select **create codespaces**
5. Wait for the container to build the first time (a few minutes; the oneAPI install is
   large). Subsequent restarts are faster.
6. When the editor opens, the `postCreateCommand` has already copied the reference data
   into `workdata/`.

> [!TIP]
> **Same container, locally (offline fallback).** The same environment runs without GitHub.
> Clone the repo, open it in VS Code with the **Dev Containers** extension, and choose
> *Reopen in Container*. Everything else in this guide is identical; the only difference is
> where the container runs. Useful for venues with poor Wi-Fi or for participants who prefer
> local disk.

### 2.4 Before the session: confirm the toolchain

Confirm the toolchain in the vscode codespaces terminal.  If you don't see a terminal at
the bottom of the codespaces, click on View and then Terminal. 

```bash
which gfortran # /usr/bin/gfortran
gfortran --version
cmake --version
ls workdata   # expect: IA_Clayton_Test_Case  my_data
```

### 2.5 First build

The repo ships `CMakePresets.json`; the Linux gfortran preset is `gfortran_debug_linux`
(with `gfortran_release_linux` for an optimised build). 

1. Begin by clicking the icon on the left that looks like triangle with a
   wrench on top of it.
2. Check under Configure to be sure that it says gfortran debug version. If it is
   empty or has something else, select the pencil icon next to it and select
   the gfortran debug version.
3. Right click on CMakeLists.txt and select Clean Reconfigure All Projects. You
   should do this step every time you change compilers or add or delete any files to the source code.
4. Right click CMakeLists.txt again and select Build All Projects or select the
   build icon at the bottom of codespaces.

> The build executable lands in the build/debug directory. Its name prints at the 
> end of the build log on step 4 above.

When vscode or codespaces is opened, CMake Tools auto-configures its compiler presets 
from the last known instance. See steps 1-2 above if you need to change the compiler.

Confirm that `src/main.f90` now exists; CMake generated it from `main.f90.in` at
configure time, you did *not* create it.  

> [!WARNING]
> **If CMake Tools configured with `ifx`.** `devcontainer.json` still pins `ifx` for the
> editor. To build `gfortran` from the GUI, set `CMAKE_Fortran_COMPILER` and `FC` to
> `gfortran` in `.vscode/settings.json`, then run *CMake: Delete Cache and Reconfigure*
> (see [Switching compilers](#53-switching-compilers-gfortran-and-ifx)). Using the command
> line above avoids this entirely.

### 2.6 First run and first debug

1. Click on codespaces explorer icon in the upper left then select the src folder and open main.f90 `and set a breakpoint at the line `call proc_read` (click in the
   gutter to set the breakpoint).
2. Select the debug icon that looks a bug on the left.  Then in top drop down, choose the **IA_Clayton_Test_Case**.
3. Press `F5` to run the binary. `gdb` starts it with the working directory set to
   `workdata/IA_Clayton_Test_Case` (per `launch.json:13`). You can also select Run then Start Debugging.
3. Execution should stop at your breakpoint. Press `F5` again to continue; the model runs
   to completion in the integrated terminal.

This proves the four things every later exercise depends on: the build works, symbols are
present, breakpoints bind, and the dataset is found.

> [!WARNING]
> **If breakpoints do not bind (grey, hollow markers).** Check that the build is **Debug**
> (`gfortran_debug_linux`, i.e. `-O0 -g`); a Release build has no line symbols, so every
> breakpoint stays hollow. Re-select the Debug variant and rebuild. With a `gfortran` Debug
> build and the bundled `gdb`, breakpoints bind and stepping works.
> 
> This is why the guide uses `gfortran`: the `ifx` build was not debuggable here. Stock
> `gdb` would not bind breakpoints in it, and pointing the launch configuration at Intel's
> own `gdb-oneapi` (via `miDebuggerPath`) did not help either. If you must build with `ifx`
> for other reasons, do the debugging on a parallel `gfortran` build.

### 2.7 Orientation: the source layout and the program shape


**Program shape (`main.f90`).** Three phases, the mental model for everything that follows:

- `call proc_read`: read every input file.
- `call proc_hru`: initialise HRUs, soils, land use, routing.
- `call time_control`: the simulation loop. Everything before is setup; everything after is
  reporting.

For a more complete call tree see [swatplus_call_tree.md](swatplus_call_tree.md).

*read → init → run.* That is enough Fortran orientation to start.

---

## 3 Exercise 1: Expose the Hargreaves exponent

**Category:** modify existing functionality. **Touches:** 5 files. **Pattern taught:** add a
calibratable basin parameter; find *every* call site before changing any.

### 3.1 Background

Many SWAT+ users work in data-scarce regions where only temperature records are available:
no wind speed, no humidity, no radiation. There the Hargreaves method (`pet == 2` in
`codes.bsn`) is the only viable PET option. But the equation was developed at a single
lysimeter site in Davis, California (Hargreaves & Samani, 1985) and its coefficients are
baked into the source. The only lever users have today is `pet_co` in `hydrology.hyd`, a
per-HRU linear multiplier. Is that enough?

### 3.2 Guided discovery

1. **Find the equation.** `Ctrl+Shift+F` for `HARGREAVES POTENTIAL`. The match is the
   comment at `et_pot.f90:259`; the equation sits just below at `et_pot.f90:267`:
   
   ```fortran
   pet_day = 0.0023 * (ramm / xl) * (w%tave + 17.8) * (w%tmax - w%tmin)**0.5
   ```

2. **Two hardcoded constants:** the linear coefficient `0.0023` and the exponent `0.5`.
   Which is worth calibrating?

3. **Discover `pet_co`.** At `et_pot.f90:280`:
   `pet_day = hru(j)%hyd%pet_co * pet_day`. This is a linear multiplier applied *after*
   the equation, so changing `pet_co` is mathematically identical to changing `0.0023`. The
   coefficient is *already* adjustable; exposing it again would be redundant.

4. **The exponent is different: it is nonlinear.** The `0.5` changes *how* PET responds to
   the diurnal temperature range, not just the magnitude. A site with a 10 °C daily swing
   and one with a 2 °C swing respond differently; no linear multiplier can replicate that.

5. **Literature.** The fixed `0.5` is an artefact of the 1985 calibration site, not a
   universal constant. Recent studies calibrate the exponent regionally and find values from
   roughly 0.3 to 0.8 depending on climate zone (References below).

> [!NOTE]
> **Teacher note.** The search term `HARGREAVES POTENTIAL EVAPOTRANSPIRATION METHOD` lands
> learners on the comment at `et_pot.f90:259`, directly above the equation at line 267. If
> someone is stuck, point them at the grep `** 0.5` in `src/`.

### 3.3 References

- Hargreaves, G.H. & Samani, Z.A. (1985). Reference crop evapotranspiration from
  temperature. *Applied Engineering in Agriculture*, 1(2), 96–99. The original equation,
  fitted to eight years of lysimeter data at *one* site. The 0.5 has no theoretical basis
  beyond that fit.
- Samani, Z. (2000). Estimating solar radiation and ET using minimum climatological data.
  *J. Irrig. Drain. Eng.*, 126(4), 265–267. Fixed constants can overestimate ET by up to
  54 %; calibration cuts error to ~15 %.
- Droogers, P. & Allen, R.G. (2002). Estimating reference ET under inaccurate data
  conditions. *Irrig. Drain. Syst.*, 16, 33–45.
- Lujano, A. et al. (2025). Regionalization of the Hargreaves–Samani coefficients in
  high-altitude areas. *Atmosphere*.
- Musa, A. et al. (2025). Extra dimensions to the calibration of the Hargreaves–Samani
  equation under data-scarce environments. *Water Resources Management*.

### 3.4 The exercise

Replace the hardcoded `0.5` with a new basin parameter `harg_expo` (default 0.5) so
modellers can calibrate it without editing source. The exponent appears in **three** places;
finding all of them is part of the exercise.

**File 1:** `basin_module.f90` (declare the parameter). Add `harg_expo` to the `basin_parms`
type, right after `co2` (line 129) and *before* the integer `day_lag_mx` (line 130):

```fortran
real :: co2 = 400.      !! co2 concentration at start of simulation (ppm)
real :: harg_expo = 0.5 !! none - Hargreaves temp-range exponent (0.3-0.8)
integer :: day_lag_mx = 0 !! max days to lag hydrographs for hru, ru and channels
```

**File 2:** `basin_prm_default.f90` (default guard). Add after the `co2` guard at line 51:

```fortran
if (bsn_prm%harg_expo < 1.e-6) bsn_prm%harg_expo = 0.5
```

**File 3:** `parameters.bsn` (input data, in `workdata/IA_Clayton_Test_Case`). Add a `harg_expo`
column header and value `0.50000` between the `co2` (column 43) and `day_lag_max` (column
44) columns on *both* the header and data lines. The struct is read in one list-directed
statement, `read (107,*) bsn_prm` at `basin_read_prm.f90:24`, so columns map *by position*
to the type declaration. The new column must sit exactly where the new field sits in File 1.

**File 4:** `et_pot.f90:267` (main Hargreaves). Replace:

```fortran
pet_day = 0.0023 * (ramm / xl) * (w%tave + 17.8) * (w%tmax - w%tmin)**0.5
```

with:

```fortran
pet_day = 0.0023 * (ramm / xl) * (w%tave + 17.8) * &
          (w%tmax - w%tmin) ** bsn_prm%harg_expo
```

`basin_module` is already imported here (`et_pot.f90:40`).

**File 5:** `climate_control.f90:187` and `206` (gap-fill and CMI Hargreaves). Replace
`** 0.5` with `** bsn_prm%harg_expo` in *both* places. `basin_module` is already imported
here too (`climate_control.f90:36`).

### 3.5 Rebuild and verify (in Codespaces)

1. Rebuild (`F7`). Because Files 1–5 are all existing `.f90` files, no CMake re-configure
   is needed; the incremental build picks them up.
2. Run with `harg_expo = 0.5` (or the column absent, where the default guard restores 0.5).
   Output must be byte-identical to the baseline run.
3. Set `harg_expo = 0.6`. Because the diurnal temperature range > 1 °C every real day,
   raising the exponent *increases* PET everywhere, but *relatively more* where the range is
   large. Large-range HRUs (continental, arid) see the biggest jump; small-range HRUs
   (coastal, humid) see a smaller one. The cross-site gradient is the signal.
4. Set `harg_expo = 0.4`. PET decreases everywhere, biggest drop at large-range sites, the
   mirror image of step 3.

| Field           | `harg_expo`                                                                                                                  |
| --------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| Default         | 0.5 (original Hargreaves & Samani, 1985)                                                                                     |
| Plausible range | 0.3 – 0.8                                                                                                                    |
| Naming          | struct suffix follows `eros_expo` (`basin_module.f90:125`); keep the column header `harg_expo` on both sides for consistency |

### 3.6 Discussion prompts

- Why can't `pet_co` replace what we just did? (`pet_co` is linear and scales all HRUs
  uniformly; the exponent reshapes the temperature-range response curve differently for each
  HRU.)
- Grep `** 0.5` across all of `src/`. Did we catch every Hargreaves site? Are there `** 0.5`
  occurrences that are *not* Hargreaves? Find all, then filter to the ones that matter.
- **Stretch:** make `harg_expo` per-HRU instead of per-basin, mirroring the `pet_co` pattern
  (`hydrology_data_module.f90` → `topohyd_init.f90` → `et_pot.f90`).

> [!IMPORTANT]
> The most common failure is putting the `harg_expo` column in the wrong position in
> `parameters.bsn`. The file is read by position, not by name; a misplaced column makes
> `co2`, `harg_expo`, and `day_lag_max` silently hold each other's values. The symptom is a
> wrong-but-not-crashing run. Re-check that the column order matches the type declaration
> order in File 1.

### 3.7 Commit and Push Your Changes

In order for your changes to persist in Github, your code changes must be staged, committed,
then pushed to Github.

1. In Codespaces, click the **git graph icon** on the left sidebar — it is the icon that
   looks like a tree with branches.
2. Click the **+** sign beside each file that was changed. This will "stage" the file to be
   committed.
3. Add a message in the text box under **Changes**, then click **Commit**. This commits your
   staged changes.
4. Finally, click the **ellipsis (...)** beside Changes and select **Push**. This pushes your
   changes to your fork on Github.

### 3.8 Shutdown Codespaces

Github will charge you for Codespaces if you use more than 60 hours with 2 processors in a
month.

> [!WARNING]
> All your code changes in Codespaces will be lost if you did not commit and push your
> changes to Github. If you have not done so, go back to
> [Section 3.7 Commit and Push Your Changes](#37-commit-and-push-your-changes) before
> shutting down.

1. In your browser, go to the Codespaces tab where you started Codespaces and click the
   **refresh/reload** icon.
2. At the bottom of the screen you should see a running Codespaces instance that has a goofy
   name. Click the **ellipsis (...)** beside it, select **Delete**, and answer **Yes** to the
   prompt.

---

## 4 Exercise 2: Add a constant daily PET method

**Category:** add new functionality (a new module). **Touches:** 1 new file + 3 existing.
**Pattern taught:** add a method to a `select case` dispatcher; the five-step recipe
struct → default → input column → new file → dispatcher case.

### 4.1 Motivation and orientation

During setup for data-poor basins it is common to want PET forced to a fixed daily value (a
climate-normal, or a sensitivity study). SWAT+ already supports four PET methods, dispatched
at `et_pot.f90:115`:

```fortran
select case (bsn_cc%pet)
  case (0) !! PRIESTLEY-TAYLOR  (line 117)
  case (1) !! PENMAN-MONTEITH   (line 150)
  case (2) !! HARGREAVES        (line 259, touched in Ex. 1)
  case (3) !! READ IN PET VALUES (line 273)
end select
```

> [!NOTE]
> **Teacher note.** There is *no* `case default`. Setting `bsn_cc%pet` to any unhandled
> value silently leaves `pet_day` at whatever it held last, a real trap. Setting `pet = 4`
> *today*, before we add `case (4)`, would already be a silent bug. Say this out loud; it
> motivates the whole exercise.

**Goal.** Add a fifth method, "constant daily PET," read from a new basin parameter
`pet_const` (mm/day), packaged as a standalone subroutine in a new file
`src/pet_constant.f90` and wired in as `case (4)`.

### 4.2 The five steps

#### Step 1: declare `pet_const` in the basin struct (same three-file pattern as Exercise 1)

*File A:* `basin_module.f90`, immediately after `co2` (line 129) and before `day_lag_mx`.
If Exercise 1 already placed `harg_expo` here, put `pet_const` right after it. Add to
`parameters.bsn`:

```fortran
real :: pet_const = 0. !! mm/day - constant PET if bsn_cc%pet == 4
```

*File B:* `basin_prm_default.f90`, after the `co2` guard (line 51). Note the guard differs
from `harg_expo`: 0 is a *legal* value (no constant applied), so clamp only negatives:

```fortran
if (bsn_prm%pet_const < 0.) bsn_prm%pet_const = 0.
```

*File C:* `parameters.bsn` (in `workdata/IA_Clayton_Test_Case`). Add a `pet_const` column with value
`0.00000` in the same position as File A, on both the header and data lines.

#### Step 2: create the new source file `src/pet_constant.f90`

```fortran
subroutine pet_constant (pet_day)

!! ~ ~ ~ PURPOSE ~ ~ ~
!! Assigns a basin-wide constant as potential ET. Used when
!! bsn_cc%pet == 4. The constant is read from parameters.bsn
!! via bsn_prm%pet_const (mm/day).

    use basin_module

    implicit none

    real, intent(out) :: pet_day !! mm/day -- potential ET

    pet_day = max (0., bsn_prm%pet_const)

    return
end subroutine pet_constant
```

No `j` (HRU index) argument is needed; a basin-wide constant does not depend on the HRU. A
per-HRU variant would add `integer, intent(in) :: j` and use it.

#### Step 3: wire it into the dispatcher

In `et_pot.f90`, between line 276 (`pet_day = wst(iwst)%weat%pet`) and line 278
(`end select`), insert:

```fortran
case (4) !! CONSTANT DAILY PET (basin-wide)
  call pet_constant (pet_day)
```

No `use` statement is needed in `et_pot` for the new subroutine; it is an external
procedure call.

#### Step 4: fix the documentation comment

Fix the documentation comment for `bsn_cc%pet` in `basin_module.f90` (lines 20–23). The
struct comment lists only methods 0–2; it was already missing 3 before our change. Extend
it:

```fortran
integer :: pet = 0 !! potential ET method code
                   !! 0 = Priestley-Taylor
                   !! 1 = Penman-Monteith
                   !! 2 = Hargreaves
                   !! 3 = read PET from file
                   !! 4 = constant (bsn_prm%pet_const)
```

Small edit, real value: a one-commit upstream PR in itself.

#### Step 5: re-configure, then build

> [!IMPORTANT]
> **Codespaces specifics for a brand-new source file.** CMake collects sources with
> `file(GLOB sources src/*.f90)` (`CMakeLists.txt:150`). A GLOB is evaluated at *configure*
> time, *not* on every build, so a brand-new `pet_constant.f90` will **not** be picked up
> by a plain `F7` rebuild. You must re-run configure first:
> 
> - **GUI:** Command Palette → *CMake: Configure*, then `F7` to build.
> - **CLI:** `cmake -S . -B build && cmake --build build -j`.
> 
> **Symptom if you forget:** the build succeeds but the linker reports *undefined reference
> to 'pet_constant_'* (or the case 4 branch is simply never compiled). The fix is always:
> re-configure.

### 4.3 Activate and verify

**Activate.** In `codes.bsn` change the `pet` column from `2` to `4`. In `parameters.bsn`,
set the new `pet_const` column to `4.00000` (mm/day).

**Expected results.**

- Daily `pet` in `hru_wb_day.txt` should be exactly 4.000 mm on every active day.
- Annual *potential* ET in `hru_wb_yr.txt` should be ≈ 4 × 365.25 ≈ 1461 mm/yr.
- Annual *actual* ET (the `et` column) will be *less* than 1461; it is bounded by soil
  water, plant stress, and snow cover. PET is a **ceiling**, not the value that lands in
  `et`. (A common point of confusion, so state it explicitly.)
- Against the baseline Hargreaves run, `basin_wb_aa.txt` should show *no* change in precip
  or runoff, but ET and soil moisture differ because the PET ceiling shifted.

**Stress test.** Set `pet_const = 0`. PET should be identically zero, soil keeps filling,
no transpiration, crop yield collapses to near zero. This confirms the dispatcher is
actually routing through your new code and not falling through to a stale value.

### 4.4 Discussion prompts

- Why keep the unused `j` argument out, when the other PET branches have HRU context? When
  is "consistency with neighbours" worth a cruft argument, and when does it hide a future
  bug?
- Where does `bsn_cc%pet` get validated? Grep and find out. If it is never validated,
  should your PR also add a range check (`0 <= pet <= 4`) in `basin_read_cc.f90`? This is
  the "while you're in there" judgment call that separates a clean PR from a sprawling one.
- **Stretch:** repeat the pattern with a new snow-melt method instead of PET (grep for the
  snowmelt dispatch; `snom.f90` has a similar `select case` shape). Pure reinforcement.

> [!IMPORTANT]
> **Pitfalls participants hit:**
> 
> - **`pet_const` column in the wrong position:** the same position-not-name trap as
>   Exercise 1.
> - **Forgot to re-configure** after adding the new file: an undefined-reference link error.
>   Re-run *CMake: Configure*.
> - **Set `pet = 4` before adding `case (4)`:** no crash, no warning, just wrong numbers
>   (the "no `case default`" trap).
> - **Declared `pet_const` as `integer`:** a type mismatch at `max(0., ...)` or silent
>   truncation of 4.0 to 4. It must be `real`.

### 4.5 Commit and Push Your Changes

See [Section 3.7 Commit and Push Your Changes](#37-commit-and-push-your-changes) for the
steps to stage, commit, and push your Exercise 2 changes to your fork.

### 4.6 Shutdown Codespaces

See [Section 3.8 Shutdown Codespaces](#38-shutdown-codespaces) for the steps to safely shut
down your Codespaces instance.

---

## 5 Quick reference

### 5.1 File and line locations (commit `5ccf6f0`)

| File                    | Line      | What                                         |
| ----------------------- | --------- | -------------------------------------------- |
| `et_pot.f90`            | 40        | `use basin_module`                           |
| `et_pot.f90`            | 115       | `select case (bsn_cc%pet)`                   |
| `et_pot.f90`            | 259       | `case (2)` HARGREAVES comment                |
| `et_pot.f90`            | 267       | Hargreaves equation (`** 0.5`)               |
| `et_pot.f90`            | 273       | `case (3)` read PET                          |
| `et_pot.f90`            | 276       | `pet_day = wst(iwst)%weat%pet`               |
| `et_pot.f90`            | 278       | `end select` (insert `case (4)` above)       |
| `et_pot.f90`            | 280       | `pet_co` multiplier                          |
| `climate_control.f90`   | 36        | `use basin_module`                           |
| `climate_control.f90`   | 187       | Hargreaves `** 0.5` (gap-fill)               |
| `climate_control.f90`   | 206       | Hargreaves `** 0.5` (CMI)                    |
| `basin_module.f90`      | 16        | `type basin_control_codes`                   |
| `basin_module.f90`      | 20–23     | `pet` method-code comment                    |
| `basin_module.f90`      | 75        | `type (basin_control_codes) :: bsn_cc`       |
| `basin_module.f90`      | 77        | `type basin_parms`                           |
| `basin_module.f90`      | 125       | `eros_expo` (naming precedent)               |
| `basin_module.f90`      | 129       | `co2` (insert new reals after)               |
| `basin_module.f90`      | 130       | `day_lag_mx` (insert before)                 |
| `basin_prm_default.f90` | 51        | `co2` guard (insert new guards after)        |
| `basin_read_prm.f90`    | 24        | `read (107,*) bsn_prm` (positional)          |
| `CMakeLists.txt`        | 131–138   | `configure_file main.f90.in → main.f90`      |
| `CMakeLists.txt`        | 150       | `file(GLOB sources src/*.f90)`               |
| `parameters.bsn`        | col 43–44 | `co2`, then `day_lag_max` (new cols between) |
| `codes.bsn`             | `pet` col | set to `4` to activate Exercise 2            |

### 5.2 Codespaces command cheat-sheet

```bash
# toolchain check
which gfortran && gfortran --version && cmake --version

# configure + build via shipped gfortran preset (recommended)
cmake --preset gfortran_debug_linux   # or gfortran_release_linux
cmake --build build/debug -j          # build/release for the release preset

# configure + build without a preset
cmake -S . -B build -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=Debug
cmake --build build -j

# run a dataset (preset build)
cd workdata/IA_Clayton_Test_Case && ../../build/debug/<swatplus-binary>

# GUI: F7 build, F5 debug (pick "IA_Clayton_Test_Case"), Ctrl+Shift+F search
# After adding a NEW .f90 file: Command Palette -> "CMake: Configure" first
```

### 5.3 Switching compilers (gfortran and ifx)

These exercises use `gfortran`. Only switch to `ifx` if you specifically want an
Intel-optimised build, and keep in mind it was not debuggable here (see the breakpoints note
in [Section 2.6](#26-first-run-and-first-debug)).

`F7` alone cannot change the compiler: CMake records `CMAKE_Fortran_COMPILER` in
`build/debug/CMakeCache.txt` at the first configure and locks it for the life of that cache,
so a rebuild keeps the same compiler. Both debug presets (`gfortran_debug_linux` and
`ifx_debug`) also write to the same `build/debug` directory, so you must delete it rather
than point at a new one. To move to `ifx` on the command line:

```bash
rm -rf build/debug               # drop the gfortran-pinned cache
cmake --preset ifx_debug         # re-configure with ifx
cmake --build build/debug -j
```

Swap the preset names to go back to `gfortran`. To keep both compilers side by side instead,
give each its own build directory (`cmake -B build/gfortran ...` and
`cmake -B build/ifx ...`); then no cache deletion is needed.

In the editor (CMake Tools), `devcontainer.json` currently pins `ifx` through
`cmake.configureSettings` and `cmake.environment`, so the GUI configures with `ifx` until
you override it: in `.vscode/settings.json` set `CMAKE_Fortran_COMPILER` and `FC` to
`gfortran`, run *CMake: Delete Cache and Reconfigure*, then `F7`. This override should be
made the default in the repo so the GUI matches the command line (tracked in the
known-issues notes).

> [!NOTE]
> `gfortran` and `ifx` do not produce byte-identical floating-point output, so the "output
> must be byte-identical to baseline" check in Exercise 1 only holds within a single
> compiler. Establish the baseline with whichever compiler you will compare against.

### 5.4 Re-verifying after a source update

If the submodule moves past `5ccf6f0`, line numbers may drift. Re-confirm the anchors
quickly:

```bash
grep -n "HARGREAVES POTENTIAL" src/et_pot.f90
grep -n "select case (bsn_cc%pet)" src/et_pot.f90
grep -n "\*\* *0\.5" src/et_pot.f90 src/climate_control.f90
grep -n ":: co2\|day_lag_mx" src/basin_module.f90
grep -n "bsn_prm%co2" src/basin_prm_default.f90
grep -n "file(GLOB\|main.f90.in" CMakeLists.txt
```
