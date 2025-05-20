import struct
import os
import numpy as np
import pandas as pd
from scipy.interpolate import interp1d
from pathlib import Path
from datetime import datetime

from matplotlib import pyplot as plt
plt.style.use("seaborn-v0_8-whitegrid")

def parse_filename_info(filename):
    """
    Extract sensor name, date, and time from the filename.
    
    Parameters
    ----------
    filename : str
        The filename to parse
    
    Returns
    -------
    dict
        Dictionary with sensor, date_deploy, and time_deploy
    """
    # Convert to string and remove extension if needed
    if isinstance(filename, Path):
        filename = filename.stem
    elif '.' in filename:
        filename = Path(filename).stem
    
    # Parse different filename formats
    if '-' in filename:
        sensor, date_time = filename.split('-')
    else:
        sensor = filename[:3]
        date_time = filename[3:]
    
    # Extract date and time
    date_str = date_time[:4]  # MMDD format
    time_str = date_time[4:]  # HHMMSS format
    
    date_deploy = datetime.strptime(date_str, "%m%d").strftime("%d/%m")
    time_deploy = f"{time_str[:2]}:{time_str[2:4]}:{time_str[4:]}"
    
    return {
        'sensor': sensor,
        'date_deploy': date_deploy,
        'time_deploy': time_deploy
    }

def read_imp_raw(filename):
    """
    Read raw IMP data file.
    
    Parameters
    ----------
    filename : str or Path
        Path to the IMP raw file
    
    Returns
    -------
    pd.DataFrame
        DataFrame with IMP data
    """
    filename = Path(filename)
    packetSize = 29
    FS = 2000
    IMU_PREC = 3
    P_PREC = 1
    T_BAT_PREC = 2
    gain_ac = 0.005 
    gain_gy = 0.1
    gain_mg = 0.1
    gain_pr = 0.1
    gain_t = 0.01
    gain_bt = 0.01
    
    with open(filename, 'rb') as file_ID:
        fstat = os.stat(filename)
        flen = (fstat.st_size // packetSize) - 1

        TimeRaw = []
        TimeSpot = []

        for _ in range(flen):
            time_raw = struct.unpack('>i', file_ID.read(4))[0]
            TimeRaw.append(time_raw)
            TimeSpot.append(file_ID.tell())
            file_ID.seek(25, 1)

        DataRaw = np.zeros((flen, 12), dtype=np.int16)
        DataRawP = np.zeros(flen, dtype=np.uint16)

        for it in range(flen):
            if it == 0:
                file_ID.seek(4, 0)
            else:
                DataRaw[it, 0] = struct.unpack('>h', file_ID.read(2))[0]
                DataRaw[it, 1] = struct.unpack('>h', file_ID.read(2))[0]
                DataRaw[it, 2] = struct.unpack('>h', file_ID.read(2))[0]
                DataRaw[it, 3] = struct.unpack('>h', file_ID.read(2))[0]
                DataRaw[it, 4] = struct.unpack('>h', file_ID.read(2))[0]
                DataRaw[it, 5] = struct.unpack('>h', file_ID.read(2))[0]
                DataRaw[it, 6] = struct.unpack('>h', file_ID.read(2))[0]
                DataRaw[it, 7] = struct.unpack('>h', file_ID.read(2))[0]
                DataRaw[it, 8] = struct.unpack('>h', file_ID.read(2))[0]
                file_ID.seek(2, 1)
                DataRaw[it, 9] = struct.unpack('>h', file_ID.read(2))[0]
                DataRaw[it, 10] = struct.unpack('>h', file_ID.read(2))[0]
                file_ID.seek(5, 1)

        DataRaw[0, :] = DataRaw[1, :]

        for it in range(flen):
            if it == 0:
                file_ID.seek(TimeSpot[0] + 4 + (2 * 7), 0)
            else:
                DataRawP[it] = struct.unpack('>H', file_ID.read(2))[0]
                file_ID.seek(27, 1)

        DataRawP[0] = DataRawP[1]

        RAPIDIMP = {
            'td': np.array(TimeRaw, dtype=np.float64),
            'ts': np.array(TimeRaw, dtype=np.float64) / FS,
            'ax': np.round(DataRaw[:, 0] * gain_ac, IMU_PREC),
            'ay': np.round(DataRaw[:, 1] * gain_ac, IMU_PREC),
            'az': np.round(DataRaw[:, 2] * gain_ac, IMU_PREC),
            'gx': np.round(DataRaw[:, 3] * gain_gy, IMU_PREC),
            'gy': np.round(DataRaw[:, 4] * gain_gy, IMU_PREC),
            'gz': np.round(DataRaw[:, 5] * gain_gy, IMU_PREC),
            'mx': np.round(DataRaw[:, 6] * gain_mg, IMU_PREC),
            'my': np.round(DataRaw[:, 7] * gain_mg, IMU_PREC),
            'mz': np.round(DataRaw[:, 8] * gain_mg, IMU_PREC),
            'p': np.round(DataRawP * gain_pr, P_PREC),
            't': np.round(DataRaw[:, 9] * gain_t, T_BAT_PREC),
            'b': np.round(DataRaw[:, 10] * gain_bt, T_BAT_PREC)
        }
        
        column_names_raw = [
            'time_s', 'inacc_x_ms', 'inacc_y_ms', 'inacc_z_ms',
            'rot_x_degs', 'rot_y_degs', 'rot_z_degs', 'mag_x_mt',
            'mag_y_mt', 'mag_z_mt', 'pressure_kpa', 'temp_c', 'battery_v'
        ]
        
        # Convert pressure from mbar to kPa during export
        dataExportCSV = np.column_stack((
            RAPIDIMP['ts'], RAPIDIMP['ax'], RAPIDIMP['ay'], RAPIDIMP['az'],
            RAPIDIMP['gx'], RAPIDIMP['gy'], RAPIDIMP['gz'], RAPIDIMP['mx'], RAPIDIMP['my'],
            RAPIDIMP['mz'], RAPIDIMP['p'] / 10.0, RAPIDIMP['t'], RAPIDIMP['b']
        ))
    
        return pd.DataFrame(dataExportCSV, columns=column_names_raw)

def read_hig_raw(filename):
    """
    Read raw HIG data file.
    
    Parameters
    ----------
    filename : str or Path
        Path to the HIG raw file
    
    Returns
    -------
    pd.DataFrame
        DataFrame with HIG data
    """
    filename = Path(filename)
    packetSize = 11
    FS = 2000
    HIG_PREC = 1
    gain_hig = 0.1
    
    with open(filename, 'rb') as file_ID:
        fstat = os.stat(filename)
        flen = fstat.st_size // packetSize

        TimeRaw = []
        DataRaw = np.zeros((flen, 3), dtype=np.int16)
        
        for it in range(flen):
            packet = file_ID.read(packetSize)
            
            time_raw = struct.unpack('>I', packet[:4])[0]
            TimeRaw.append(time_raw)

            DataRaw[it, 0] = struct.unpack('>h', packet[4:6])[0]  # acc X
            DataRaw[it, 1] = struct.unpack('>h', packet[6:8])[0]  # acc Y
            DataRaw[it, 2] = struct.unpack('>h', packet[8:10])[0]  # acc Z
            
        TimeRaw = np.array(TimeRaw, dtype=np.float64)
        ts = TimeRaw / FS
        ax = np.round(DataRaw[:, 0] * gain_hig, HIG_PREC)
        ay = np.round(DataRaw[:, 1] * gain_hig, HIG_PREC)
        az = np.round(DataRaw[:, 2] * gain_hig, HIG_PREC)
        aMag = np.round(np.sqrt(ax ** 2 + ay ** 2 + az ** 2), HIG_PREC)

        column_names_raw = [
            'time_s', 'higacc_x_g', 'higacc_y_g', 'higacc_z_g', 'higacc_mag_g'
        ]
        
        dataExportCSV = np.column_stack((ts, ax, ay, az, aMag))
        return pd.DataFrame(dataExportCSV, columns=column_names_raw)

def process_imp_hig_direct(imp_filename, hig_filename, output_dir):
    """
    Directly process IMP and HIG raw files into a merged dataset.
    
    Parameters
    ----------
    imp_filename : str or Path
        Path to the IMP raw file
    hig_filename : str or Path
        Path to the HIG raw file
    output_dir : str or Path, optional
        Directory to save the combined CSV, by default "./RAPID_Processed"
    
    Returns
    -------
    tuple
        (pd.DataFrame, dict) - The merged dataset and summary information
    """
    # Read raw data directly
    imp_data = read_imp_raw(imp_filename)
    hig_data = read_hig_raw(hig_filename)
    
    # Get base filename
    base_filename = Path(imp_filename).stem
    
    # Extract file info from the filename
    file_info = parse_filename_info(base_filename)
    
    # Create a uniform 2000Hz time series
    start_time = imp_data["time_s"].min()
    end_time = imp_data["time_s"].max()
    time_step = 1.0 / 2000  # 0.0005 seconds
    times = np.arange(start_time, end_time + time_step, time_step)
    
    # Create combined dataset with the high-resolution time series
    combined_data = pd.DataFrame({"time_s": times})
    
    # Add HIG data through nearest-neighbor mapping
    for col in hig_data.columns:
        if col != "time_s":
            combined_data[col] = 0.0
    
    for i, t in enumerate(hig_data["time_s"]):
        idx = np.abs(combined_data["time_s"] - t).argmin()
        for col in hig_data.columns:
            if col != "time_s":
                combined_data.loc[idx, col] = hig_data.loc[i, col]
    
    # Interpolate IMP data to the high-resolution time series
    for col in imp_data.columns:
        if col != "time_s":
            interp_func = interp1d(
                imp_data["time_s"],
                imp_data[col],
                kind='linear',
                bounds_error=False,
                fill_value="extrapolate"
            )
            combined_data[col] = interp_func(combined_data["time_s"])
    
    # Apply post-processing (pressure conversion, etc.)
    combined_data, summary_info = post_process_combined(combined_data)
    
    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)
    
    # Create CSV subdirectory
    csv_dir = output_path / "csv"
    csv_dir.mkdir(parents=True, exist_ok=True)
    
    # Save the combined data to CSV directory
    output_file = csv_dir / f"{base_filename}.csv"
    combined_data.to_csv(output_file, index=False)

    # Create and save minimal CSV
    minimal_data = create_minimal_csv(combined_data, base_filename, output_path)
    
    # Create overview plot using minimal data
    plot_combined_overview(minimal_data, base_filename, output_path)
    
    return combined_data, {**file_info, **summary_info}

def post_process_combined(data):
    """
    Apply post-processing to the combined dataset.
    """
    # Calculate acceleration magnitude for IMP data
    data["inacc_mag_ms"] = np.sqrt(
        data["inacc_x_ms"]**2 + 
        data["inacc_y_ms"]**2 + 
        data["inacc_z_ms"]**2
    ) - 9.81  # Subtract Earth's gravity
    
    # Calculate rotational magnitude
    data["rot_mag_degs"] = np.sqrt(
        data["rot_x_degs"]**2 + 
        data["rot_y_degs"]**2 + 
        data["rot_z_degs"]**2
    )
    
    # Calculate duration
    num_seconds = len(data) / 2000  # 2000Hz sampling rate
    minutes, seconds = divmod(num_seconds, 60)
    duration = f"{int(minutes):02}:{int(seconds):02}"
    
    # Find max acceleration time
    acc_max_index = data["higacc_mag_g"].idxmax()
    acc_max_time = data["time_s"][acc_max_index]
    max_acc_g_force = data["higacc_mag_g"].iloc[acc_max_index]
    
    # Find pressure nadir around acceleration maxima
    pres_min_value = None
    pres_min_time = None
    
    if "pressure_kpa" not in data.columns or data["pressure_kpa"].isna().all():
        # Handle missing pressure data by creating a column of zeros
        data["pressure_kpa"] = 0.0
        warnings.append("PRES: No pressure data available")
        pres_min_value = 0.0
        pres_min_time = acc_max_time  # Default to acceleration max time
    else:
        # Find pressure minimum within 3s of max acceleration
        window_start = max(0, acc_max_index - 3000)  # 1.5s before
        window_end = min(len(data) - 1, acc_max_index + 3000)  # 1.5s after
        pressure_window = data["pressure_kpa"].iloc[window_start:window_end]
        pres_min_local_index = pressure_window.idxmin()
        pres_min_time = data["time_s"][pres_min_local_index]
        pres_min_value = data["pressure_kpa"].iloc[pres_min_local_index]
    
    warnings = []
    
    # Check if pressure is unchanging within the window (fault condition)
    if pressure_window.nunique() <= 1:
        warnings.append("PRES: Pressure fault identified (unchanging values)")
    
    # Check for time series issues using a value which exceeds possible time
    if data["time_s"].max() >= 5000:
        warnings.append("TIME: Time series incorrect")
    
    # check for strike/collision event
    if max_acc_g_force >= 400:
        warnings.append("HIG: High impact event >= 400g found")
    
    warning_message = "; ".join(warnings) if warnings else "No warnings"
    
    summary_info = {
        "duration[mm:ss]": duration,
        "pres_min[kPa]": pres_min_value,
        "pres_min[time]": pres_min_time,
        "HIG_max[g]": max_acc_g_force,
        "HIG_max[time]": acc_max_time,
        "messages": warning_message
    }
    
    return data, summary_info
  
def create_minimal_csv(data, filename, output_dir):
    """
    Create a minimal CSV with only selected columns.
    """
    minimal_cols = ["time_s", "pressure_kpa", "higacc_mag_g", "inacc_mag_ms", "rot_mag_degs"]
    minimal_data = data[minimal_cols].copy()
    
    
    output_path = Path(output_dir) / "csv"
    output_path.mkdir(parents=True, exist_ok=True)
    
    # Save to the csv subdirectory
    output_file = output_path / f"{filename}_min.csv"
    minimal_data.to_csv(output_file, index=False)

    return minimal_data
  
def plot_combined_overview(minimal_data, filename, output_dir, save=True, show=False):
    """
    Create a 3-panel plot showing pressure with HIG acceleration, inertial acceleration, 
    and rotational magnitude.
    
    Parameters
    ----------
    minimal_data : pd.DataFrame
        The minimal dataset
    filename : str or Path
        Filename to use for saving the plot
    output_dir : str or Path
        Directory to save the plot
    save : bool, optional
        Whether to save the plot, by default True
    show : bool, optional
        Whether to show the plot, by default False
    """
    # Get data for plotting
    t = minimal_data["time_s"]
    pres = minimal_data["pressure_kpa"]
    hig_acc = minimal_data["higacc_mag_g"]
    inert_acc = minimal_data["inacc_mag_ms"]
    rot_mag = minimal_data["rot_mag_degs"]
    
    # Create figure with 3 vertically stacked subplots
    fig, (ax1, ax2, ax3) = plt.subplots(3, 1, figsize=(25, 15), sharex=True)
    
    # Panel 1: Pressure and HIG Acceleration
    # Primary y-axis (pressure)
    ax1.set_ylabel("Pressure [kPa]", color='black')
    ax1.plot(t, pres, color='black', linewidth=1.5)
    ax1.tick_params(axis='y', labelcolor='black')
    ax1.ticklabel_format(useOffset=False)
    ax1.grid(True, alpha=0.3)
    
    # Secondary y-axis (HIG acceleration)
    ax1_twin = ax1.twinx()
    ax1_twin.set_ylabel("HIG Acceleration [g]", color='red')
    ax1_twin.plot(t, hig_acc, color='red', linewidth=1.2)
    ax1_twin.tick_params(axis='y', labelcolor='red')
    
    # Panel 2: Pressure and Inertial Acceleration
    # Primary y-axis (pressure)
    ax2.set_ylabel("Pressure [kPa]", color='black')
    ax2.plot(t, pres, color='black', linewidth=1.5)
    ax2.tick_params(axis='y', labelcolor='black')
    ax2.ticklabel_format(useOffset=False)
    ax2.grid(True, alpha=0.3)
    
    # Secondary y-axis (inertial acceleration)
    ax2_twin = ax2.twinx()
    ax2_twin.set_ylabel("Inertial Acceleration [m/s²]", color='blue')
    ax2_twin.plot(t, inert_acc, color='blue', linewidth=1.2)
    ax2_twin.tick_params(axis='y', labelcolor='blue')
    
    # Panel 3: Pressure and Rotational Magnitude
    # Primary y-axis (pressure)
    ax3.set_ylabel("Pressure [kPa]", color='black')
    ax3.plot(t, pres, color='black', linewidth=1.5)
    ax3.tick_params(axis='y', labelcolor='black')
    ax3.ticklabel_format(useOffset=False)
    ax3.grid(True, alpha=0.3)
    
    # Secondary y-axis (rotational magnitude)
    ax3_twin = ax3.twinx()
    ax3_twin.set_ylabel("Rotational Magnitude [deg/s]", color='green')
    ax3_twin.plot(t, rot_mag, color='green', linewidth=1.2)
    ax3_twin.tick_params(axis='y', labelcolor='green')
    
    # Set common x-axis label
    ax3.set_xlabel("Time [s]")
    
    # Add title
    fig.suptitle(f"RAPID: {filename}", fontsize=16)
    
    # Adjust layout for better spacing
    plt.tight_layout()
    plt.subplots_adjust(top=0.95)
    
    # Save and/or show the plot
    if save:
        plots_dir = Path(output_dir) / "plots"
        plots_dir.mkdir(parents=True, exist_ok=True)
        
        output_file = plots_dir / f"{filename}_overview.png"
        plt.savefig(output_file, dpi=150)
        
    if show:
        plt.show()
        
    plt.close()

if __name__ == "__main__":
    output_dir = Path("./RAPID_Processed")
    
    current_date = datetime.now().strftime("%d%m%y")
    summary_data = []
    n_files_w_time = 0
    n_files_w_hig = 0
    n_files_w_pres = 0

    # Find IMP and HIG file pairs
    imp_files = list(Path("./RAW_data/RAPID").glob("*.IMP"))
    hig_files = list(Path("./RAW_data/RAPID").glob("*.HIG"))
    
    print(f"Found {len(imp_files)} IMP files and {len(hig_files)} HIG files")
    
    # Process matching pairs
    for imp_file in imp_files:
        # Get the base name without extension
        imp_base = imp_file.stem
        
        # Look for a matching HIG file
        matching_hig_files = [f for f in hig_files if f.stem == imp_base]
        
        if matching_hig_files:
            hig_file = matching_hig_files[0]
            print(f"Processing sensor: {imp_base}")
            
            try:
                # Process the pair directly
                combined_data, summary_info = process_imp_hig_direct(imp_file, hig_file, output_dir)
                
                # Check for errors
                if "TIME:" in summary_info["messages"]:
                    n_files_w_time += 1
                
                if "HIG:" in summary_info["messages"]:
                    n_files_w_hig += 1
                
                if "PRES:" in summary_info["messages"]:
                    n_files_w_pres += 1
                
                # Add to summary data
                summary_data.append({
                    'file': f"{imp_base}",
                    'class': '2000_combined',
                    **summary_info
                })
                
                print(f"{imp_base} processed")
                
            except Exception as e:
                print(f"Error processing {imp_base}: {e}")
    
    # Create summary CSV
    summary_df = pd.DataFrame(summary_data)
    summary_csv_filename = f"{current_date}_batch_summary.csv"
    summary_csv_path = Path(output_dir) / summary_csv_filename
    summary_df.to_csv(summary_csv_path, index=False)
    
    print("Batch sensor processing complete.")
    print(f"{len(summary_data)} total sensors processed")
    print(f"{n_files_w_pres}/{len(summary_data)} sensors contain pressure data errors")
    print(f"{n_files_w_time}/{len(summary_data)} sensors contain time series errors")
    print(f"{n_files_w_hig}/{len(summary_data)} sensors contain strike/collision event (HIG ≥ 400g)")
  
