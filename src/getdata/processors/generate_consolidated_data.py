import pandas as pd


def generate_difference(df, strat_column=2, copy_previous=True):
    out_df = pd.DataFrame()
    if copy_previous:
        out_df = df.iloc[:, :strat_column]

    df_n_columns = df.shape[1]
    for i in range(strat_column + 1, df_n_columns):
        column_name = df.columns[i]
        out_df[column_name] = df.iloc[:, i] - df.iloc[:, i - 1]

    return out_df


def df_to_long(df, value_name, id_vars=["codigo", "region"], var_name="fecha"):
    return df.melt(id_vars=id_vars, var_name=var_name, value_name=value_name)

def df_comuna_to_long(df, value_name, id_vars=["codigo_region", "region", "codigo_comuna", "comuna"], var_name="fecha"):
    return df.melt(id_vars=id_vars, var_name=var_name, value_name=value_name)

def generate():
    confirmados_file = "../../../csv/confirmados.csv"
    muertes_file = "../../../csv/muertes.csv"
    uci_file = "../../../csv/pacientes_en_uci.csv"
    pcrs_file = "../../../csv/pcrs_region.csv"

    confirmados_df = pd.read_csv(confirmados_file)
    nuevos_df = generate_difference(confirmados_df, 2, True)
    uci_df = pd.read_csv(uci_file)
    pcrs_df = pd.read_csv(pcrs_file)
    muertes_df = pd.read_csv(muertes_file)
    nuevos_muertes_df = generate_difference(muertes_df, 2, True)

    confirmados_long = df_to_long(confirmados_df, "positivos_acumulados")
    nuevos_long = df_to_long(nuevos_df, "positivos_nuevos")
    uci_long = df_to_long(uci_df, "en_uci")
    pcrs_long = df_to_long(pcrs_df, "nuevos_pcrs_informados_lab")
    muertes_long = df_to_long(muertes_df, "muertes_acumuladas")
    nuevos_muertes_long = df_to_long(nuevos_muertes_df, "muertes_nuevas")

    join_columns = ["codigo", "region", "fecha"]
    df = pd.merge(confirmados_long, nuevos_long, how="left", on=join_columns)
    df = pd.merge(df, uci_long, how="left", on=join_columns)
    df = pd.merge(df, pcrs_long, how="left", on=join_columns)
    df = pd.merge(df, muertes_long, how="left", on=join_columns)
    df = pd.merge(df, nuevos_muertes_long, how="left", on=join_columns)

    df.to_csv("../../../csv/long_regiones.csv", index=False, float_format="%.0f")

    cci_file = "../../../csv/confirmados_comunas_interpolado.csv"
    cci_df = pd.read_csv(cci_file)
    cci_long = df_comuna_to_long(cci_df, "positivos_acumulados")
    cci_long.to_csv("../../../csv/long_confirmados_comunas_interpolado.csv", index=False, float_format="%.0f")

if __name__ == "__main__":
    generate()
