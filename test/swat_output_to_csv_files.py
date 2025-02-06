import pandas as pd

output_seperator = "\t"

default_spec = {
            "skiprows": [0, 2],
            "header": [0],
            "column_names": None,
            "delim_whitespace": True
            },

default_spec2 = {
            "skiprows": [0],
            "header": [0],
            "column_names": None,
            "delim_whitespace": True
            },

spec_dict = {
        "files_out.out": {
            "skiprows": [0],
            "header": None,
            "column_names": ["type", "filename"],
            "delim_whitespace": True
            },
        "hru_orgc.txt": default_spec[0],
        "checker.out": {
            "skiprows": 3,
            "header": None,
            "column_names": ["sname", "hydgrp", "zmx", "usle_k", "sumfc", "sumul", "usle_p", 
                             "usle_ls", "esco", "epco", "cn3_swf", "perco", "latq_co", "tiledrain"],
            "delim_whitespace": True
            },
        "hru_wb_aa.txt": default_spec[0],
        "hru_ncycle_aa.txt": default_spec[0],
        "hru_nb_aa.txt": default_spec[0],
        "hru_carbon_aa.txt": default_spec[0],
        "hru_nut_carb_gl_aa.txt": default_spec[0],
        "hru_ls_aa.txt": default_spec[0],
        "hru_pw_aa.txt": default_spec[0],
        "basin_wb_aa.txt": default_spec[0],
        "basin_nb_aa.txt": default_spec[0],
        "basin_carbon_aa.txt": default_spec[0],
        "basin_ls_aa.txt": default_spec[0],
        "basin_pw_aa.txt": default_spec[0],
        "crop_yld_yr.txt": {
            "skiprows": 2,
            "header": [0],
            "column_names": None,
            "delim_whitespace": True
            },
        "crop_yld_aa.txt": {
            "skiprows": 2,
            "header": [0],
            "column_names": None,
            "delim_whitespace": True
            },
        "lu_change_out.txt": default_spec2[0],
        "basin_crop_yld_yr.txt": default_spec2[0],
        "basin_crop_yld_aa.txt": default_spec2[0],
        "hydout_aa.txt": default_spec[0],
        "hydin_aa.txt": default_spec[0],
        "deposition_aa.txt": default_spec[0],
        "wetland_aa.txt": default_spec[0],
        "basin_aqu_aa.txt": default_spec[0],
        "basin_res_aa.txt": default_spec[0],
        "recall_aa.txt": default_spec[0],
        "basin_cha_aa.txt": default_spec[0],
        "basin_sd_cha_aa.txt": default_spec[0],
        "basin_sd_chamorph_aa.txt": default_spec[0],
        "basin_psc_aa.txt": default_spec[0],
        "ru_aa.txt": default_spec[0],
        }
# print(spec_dict)
# exit(0)


def read_output(fname, spec_dict, write_csv = False):
    print(fname)
    if fname in spec_dict:
        df = None
        skiprows = spec_dict[fname]["skiprows"]
        header = spec_dict[fname]["header"]
        column_names = spec_dict[fname]["column_names"] 
        delim_whitespace = spec_dict[fname]["delim_whitespace"]
        if header is not None:
            try:
                df = pd.read_csv(fname, skiprows=skiprows, delim_whitespace=delim_whitespace, header=header)
            except FileNotFoundError:
                print(f"Swat+ output filename {fname} not found.")
                exit(1)
            except Exception as e:
                print(f"Swat+ output filename {fname} could not be parsed for some reason.")
                print(f"The error is: {e}")
                exit(1)
        if header is None and column_names is not None:
            try:
                df = pd.read_csv(fname, skiprows=skiprows, delim_whitespace=delim_whitespace, names=column_names)
            except FileNotFoundError:
                print(f"Swat+ output filename {fname} not found.")
                exit(1)
            except Exception as e:
                print(f"Swat+ output filename {fname} could not be parsed for some reason.")
                print(f"The error is: {e}")
                exit(1)

        if write_csv:
            if df is not None:
                output_file_name = fname + ".csv"
                df.to_csv(output_file_name, sep=output_seperator, index = False)
    else:
        print("Swat+ output filename not found in read specification dictionary.")
        exit(1)
    return df


def run():
    fname = "files_out.out"
    df = read_output(fname, spec_dict, True)
    for index, row in df.iterrows():
        fname = row["filename"]
        if fname == "mgt_out.txt" or fname == "yield.out":
            continue
        df = read_output(fname, spec_dict, True)
    # fname = "hru_orgc.txt"
    # fname = "hru_wb_aa.txt"
    # fname = "hru_ncycle_aa.txt"
    # fname = "hru_nb_aa.txt"
    # fname = "hru_carbon_aa.txt"
    # fname = "hru_nut_carb_gl_aa.txt"
    # fname = "hru_ls_aa.txt"
    # fname = "hru_pw_aa.txt"
    # fname = "hru_pw_aa.txt"
    # fname = "basin_wb_aa.txt"
    # fname = "basin_nb_aa.txt"
    # fname = "basin_carbon_aa.txt"
    # fname = "basin_ls_aa.txt"
    # fname = "basin_pw_aa.txt"
    # fname = "crop_yld_yr.txt"
    # fname = "crop_yld_aa.txt"
    # fname = "lu_change_out.txt"
    # fname = "basin_crop_yld_yr.txt"
    # fname = "basin_crop_yld_aa.txt"
    # fname = "hydout_aa.txt"
    # fname = "hydin_aa.txt"
    # fname = "deposition_aa.txt"
    # fname = "wetland_aa.txt"
    # fname = "basin_aqu_aa.txt"
    # fname = "basin_res_aa.txt"
    # fname = "recall_aa.txt"
    # fname = "basin_cha_aa.txt"
    # fname = "basin_sd_cha_aa.txt"
    # fname = "basin_sd_chamorph_aa.txt"
    # fname = "basin_psc_aa.txt"
    # fname = "ru_aa.txt"
    # fname = "files_out.out"
    # df = read_output(fname, spec_dict, True)


if __name__ == "__main__":
    run()
