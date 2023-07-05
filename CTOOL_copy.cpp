using System.Data;
using Ctool.Helpers;
using Ctool.variables;

namespace Ctool
{
public class CTool
{
  private Temperature Temperatures { get; set; }
  private Data Data { get; set; }
  private Variables Variables { get; set; }
  private Mode Mode { get; set; }
  
  private const int NUMBER_OF_LAYERS = 2;
  private const bool WITH_TRANSPORT = true;
  private const double t = 1.0;
  
  private readonly string OUTPUT_FOLDER = Path.Join(HelperDAO.CurrentDir, "outputFiles");
  
  private int temperatureValuePosition = 0;
  private double julianDay;
  
  internal CTool(Temperature temperatures, Data data, Variables variables, Mode mode)
  {
    Temperatures = temperatures;
    Data = data;
    Variables = variables;
    Mode = mode;
  }
  
  internal DataSet Main()
  {
    
#region DAO objects
    StreamWriter transportFile = null;
    StreamWriter CO2 = null;
    StreamWriter totalAmount = null;
    
    DataSet returnSet = new DataSet();
    DataTable transportBack = new DataTable("transport");
    DataTable CO2Back = new DataTable("CO2");
    DataTable totalAmountBack = new DataTable("totalAmount");
    
    if (Mode.Value == 2)
    {
      totalAmount = new StreamWriter(Path.Join(OUTPUT_FOLDER, "totalAmount.txt"));
      CO2 = new StreamWriter(Path.Join(OUTPUT_FOLDER, "CO2.txt"));
      HelperDAO.CreateMode2Columns(totalAmount, CO2);
      if (totalAmount is null || CO2 is null)
      {
        Console.WriteLine("Problem creating files.");
        Console.WriteLine("Press any key to exit.");
        Console.ReadLine();
        Environment.Exit(0);
      }
      
    }
    if (Mode.Value == 0 || Mode.Value == 1)
    {
      totalAmount = new StreamWriter(Path.Join(OUTPUT_FOLDER, "totalAmount.txt"));
      CO2 = new StreamWriter(Path.Join(OUTPUT_FOLDER, "CO2.txt"));
      transportFile = new StreamWriter(Path.Join(OUTPUT_FOLDER, "transport.txt"));
      if (totalAmount is null || CO2 is null || transportFile is null)
      {
        Console.WriteLine("Problem creating files.");
        Console.WriteLine("Press any key to exit.");
        Console.ReadLine();
        Environment.Exit(0);
      }
      HelperDAO.CreateMode0or1Columns(Mode.Value, totalAmount, CO2, transportFile);
    }
    if (Mode.Value == 3)
    {
      HelperDAO.CreateMode3Columns(totalAmountBack, CO2Back, transportBack);
    }
#endregion
    
    double extraCarbon = Variables.ExtraCarbon / 12;
    double CNfraction = Variables.CNFraction();
    
    double startAmountLayer1 = Variables.StartAmountOfCarbon * 0.47;
    double startAmountLayer2 = Variables.StartAmountOfCarbon * 0.53;
    
    double[] fomcPlant = new double[NUMBER_OF_LAYERS];
    double[] humcPlant = new double[NUMBER_OF_LAYERS];
    double[] romcPlant = new double[NUMBER_OF_LAYERS];
    
    double[] fomcManure = new double[NUMBER_OF_LAYERS];
    double[] humcManure = new double[NUMBER_OF_LAYERS];
    double[] romcManure = new double[NUMBER_OF_LAYERS];
    
    double[] fomcPlantC14 = new double[NUMBER_OF_LAYERS];
    double[] humcPlantC14 = new double[NUMBER_OF_LAYERS];
    double[] romcPlantC14 = new double[NUMBER_OF_LAYERS];
    
    double[] fomcManureC14 = new double[NUMBER_OF_LAYERS];
    double[] humcManureC14 = new double[NUMBER_OF_LAYERS];
    double[] romcManureC14 = new double[NUMBER_OF_LAYERS];
    
    humcPlant[0] = startAmountLayer1 * Variables.PUpperLayer * CNfraction;
    romcPlant[0] = startAmountLayer1 - humcPlant[0];
    humcPlant[1] = startAmountLayer2 * Variables.PLowerLayer * CNfraction;
    romcPlant[1] = startAmountLayer2 - humcPlant[1];
    humcPlantC14[0] = (startAmountLayer1) * Variables.PUpperLayer * (Variables.C14percent / 100) * CNfraction;
    romcPlantC14[0] = (startAmountLayer1) * (Variables.C14percent / 100) - humcPlantC14[0];
    humcPlantC14[1] = (startAmountLayer2) * Variables.PLowerLayer * (Variables.C14percent / 100) * CNfraction;
    romcPlantC14[1] = (startAmountLayer2) * (Variables.C14percent / 100) - humcPlantC14[1];
    
    foreach (Data.YearValues dataYearValues in Data.Values)
    {
      for (int month = 1; month <= 12; month++)
      {
        julianDay = month * 30.4166;
        
#region summerMonths
        if (month == 3)
        {
          fomcManure[0] = fomcManure[0] + (dataYearValues.ManureC * (1 - Variables.HUMfractionManure));
          humcManure[0] = humcManure[0] + (dataYearValues.ManureC * Variables.HUMfractionManure);
          fomcManureC14[0] = fomcManureC14[0] + ((dataYearValues.ManureC * (1 - Variables.HUMfractionManureC14))) * (dataYearValues.C14Manure / 100);
          humcManureC14[0] = humcManureC14[0] + ((dataYearValues.ManureC * Variables.HUMfractionManureC14)) * (dataYearValues.C14Manure / 100);
        }
        fomcPlant[0] = fomcPlant[0] + extraCarbon;
        if (month == 4)
        {
          fomcPlant[0] = fomcPlant[0] + (dataYearValues.UpperLayerC) * 0.08;
          fomcPlant[1] = fomcPlant[1] + (dataYearValues.LowerLayerC) * 0.08;
          
          fomcPlantC14[0] = fomcPlantC14[0] + (dataYearValues.UpperLayerC * (dataYearValues.C14PlantProcent / 100)) * 0.08;
          fomcPlantC14[1] = fomcPlantC14[1] + (dataYearValues.LowerLayerC * (dataYearValues.C14PlantProcent / 100)) * 0.08;
        }
        if (month == 5)
        {
          fomcPlant[0] = fomcPlant[0] + (dataYearValues.UpperLayerC) * 0.12;
          fomcPlant[1] = fomcPlant[1] + (dataYearValues.LowerLayerC) * 0.12;
          fomcPlantC14[0] = fomcPlantC14[0] + (dataYearValues.UpperLayerC * (dataYearValues.C14PlantProcent / 100)) * 0.12;
          fomcPlantC14[1] = fomcPlantC14[1] + (dataYearValues.LowerLayerC * (dataYearValues.C14PlantProcent / 100)) * 0.12;
        }
        if (month == 6)
        {
          fomcPlant[0] = fomcPlant[0] + (dataYearValues.UpperLayerC) * 0.16;
          fomcPlant[1] = fomcPlant[1] + (dataYearValues.LowerLayerC) * 0.16;
          fomcPlantC14[0] = fomcPlantC14[0] + (dataYearValues.UpperLayerC * (dataYearValues.C14PlantProcent / 100)) * 0.16;
          fomcPlantC14[1] = fomcPlantC14[1] + (dataYearValues.LowerLayerC * (dataYearValues.C14PlantProcent / 100)) * 0.16;
        }
        if (month == 7)
        {
          fomcPlant[0] = fomcPlant[0] + (dataYearValues.UpperLayerC) * 0.64;
          fomcPlant[1] = fomcPlant[1] + (dataYearValues.LowerLayerC) * 0.64;
          fomcPlantC14[0] = fomcPlantC14[0] + (dataYearValues.UpperLayerC * (dataYearValues.C14PlantProcent / 100)) * 0.64;
          fomcPlantC14[1] = fomcPlantC14[1] + (dataYearValues.LowerLayerC * (dataYearValues.C14PlantProcent / 100)) * 0.64;
        }
#endregion
        
        double RfractionPlant = R(Variables.ClayfractionPlant);
        double humificationPlant = 1 / (RfractionPlant + 1);
        double transportHumPlant = 0;
        double transportHumManure = 0;
        double transportFomPlant = 0;
        double transportFomManure = 0;
        double transportRomPlant = 0;
        double transportRomManure = 0;
        
        double[] co2FomPlant = DecompositionFom(ref fomcPlant, Variables.FOMdecompositionratePlant, Variables.TFPlant, humificationPlant, ref humcPlant, WITH_TRANSPORT, false, 0, ref transportFomPlant);
        if (Mode.Value == 2)
        {
          CO2.Write((co2FomPlant[0]).ToString() + '\t');
          CO2.Write((co2FomPlant[1]).ToString() + '\t');
        }
        double[] co2HumPlant = DecompositionHum(Variables.HUMdecompositionratePlant, ref humcPlant, Variables.ROMfractionPlant, ref romcPlant, WITH_TRANSPORT, false, 0, ref transportHumPlant);
        double[] co2RomPlant = DecompositionRom(ref romcPlant, Variables.ROMdecompositionratePlant, WITH_TRANSPORT, false, 0, ref transportRomPlant);
        if (Mode.Value == 2)
        {
          CO2.Write((co2HumPlant[0] + co2RomPlant[0]).ToString() + '\t');
          CO2.Write((co2HumPlant[1] + co2RomPlant[1]).ToString() + '\t');
        }
        double RfractionManure = R(Variables.ClayfractionManure);
        double humificationManure = 1 / (RfractionManure + 1);
        double[] co2FomManure = DecompositionFom(ref fomcManure, Variables.FOMdecompositionrateManure, Variables.TFManure, humificationManure, ref humcManure, WITH_TRANSPORT, false, 0, ref transportFomManure);
        if (Mode.Value == 2)
        {
          CO2.Write((co2FomManure[0]).ToString() + '\t');
          CO2.Write((co2FomManure[1]).ToString() + '\t');
        }
        double[] co2HumManure = DecompositionHum(Variables.HUMdecompositionrateManure, ref humcManure, Variables.ROMfractionManure, ref romcManure, WITH_TRANSPORT, false, 0, ref transportHumManure);
        double[] co2RomManure = DecompositionRom(ref romcManure, Variables.ROMdecompositionrateManure, WITH_TRANSPORT, false, 0, ref transportRomManure);
        if (Mode.Value == 2)
        {
          CO2.Write((co2HumManure[0] + co2RomManure[0]).ToString() + '\t');
          CO2.Write((co2HumManure[1] + co2RomManure[1]).ToString() + '\t');
        }
        if (Mode.Value == 0 || Mode.Value == 1)
        {
          CO2.Write((co2FomPlant[0] + co2FomManure[0]).ToString() + '\t');
          CO2.Write((co2FomPlant[1] + co2FomManure[1]).ToString() + '\t');
          CO2.Write((co2HumPlant[0] + co2HumManure[0]).ToString() + '\t');
          CO2.Write((co2HumPlant[1] + co2HumManure[1]).ToString() + '\t');
          CO2.Write((co2RomPlant[0] + co2RomManure[0]).ToString() + '\t');
          CO2.WriteLine((co2RomPlant[1] + co2RomManure[1]).ToString());
          transportFile.Write((transportFomPlant + transportFomManure).ToString() + '\t');
          transportFile.Write((transportHumPlant + transportHumManure).ToString() + '\t');
          transportFile.WriteLine((transportRomPlant + transportRomManure).ToString());
          
        }
        if (Mode.Value != 1)
        {
          double RfractionManureC14 = R(Variables.ClayfractionManureC14);
          double humificationManureC14 = 1 / (RfractionManureC14 + 1);
          double RfractionPlantC14 = R(Variables.ClayfractionPlantC14);
          double humificationPlantC14 = 1 / (RfractionPlantC14 + 1);
          double[] co2Fom = DecompositionFom(ref fomcPlantC14, Variables.FOMdecompositionratePlantC14, Variables.TFPlantC14, humificationPlantC14, ref humcPlantC14, WITH_TRANSPORT, true, Variables.DecayRateC14Plant, ref transportFomPlant);
          if (Mode.Value == 2)
          {
            CO2.Write((co2Fom[0]).ToString() + '\t');
            CO2.Write((co2Fom[1]).ToString() + '\t');
          }
          double[] co2Hum = DecompositionHum(Variables.HUMdecompositionratePlantC14, ref humcPlantC14, Variables.ROMfractionPlantC14, ref romcPlantC14, WITH_TRANSPORT, true, Variables.DecayRateC14Plant, ref transportHumPlant);
          double[] co2Rom = DecompositionRom(ref romcPlantC14, Variables.ROMdecompositionratePlantC14, WITH_TRANSPORT, true, Variables.DecayRateC14Plant, ref transportRomPlant);
          if (Mode.Value == 2)
          {
            CO2.Write((co2Hum[0] + co2Rom[0]).ToString() + '\t');
            CO2.Write((co2Hum[1] + co2Rom[1]).ToString() + '\t');
          }
          co2Fom = DecompositionFom(ref fomcManureC14, Variables.FOMdecompositionrateManureC14, Variables.TFManureC14, humificationManureC14, ref humcManureC14, WITH_TRANSPORT, true, Variables.DecayRateC14Manuer, ref transportFomManure);
          if (Mode.Value == 2)
          {
            CO2.Write((co2Fom[0]).ToString() + '\t');
            CO2.Write((co2Fom[1]).ToString() + '\t');
          }
          co2Hum = DecompositionHum(Variables.HUMdecompositionrateManureC14, ref humcManureC14, Variables.ROMfractionManureC14, ref romcManureC14, WITH_TRANSPORT, true, Variables.DecayRateC14Manuer, ref transportHumManure);
          co2Rom = DecompositionRom(ref romcManureC14, Variables.ROMdecompositionrateManureC14, WITH_TRANSPORT, true, Variables.DecayRateC14Manuer, ref transportRomManure);
          if (Mode.Value == 2)
          {
            CO2.Write((co2Hum[0] + co2Rom[0]).ToString() + '\t');
            CO2.WriteLine((co2Hum[1] + co2Rom[1]).ToString() + '\t');
          }
        }
        if (Mode.Value == 2)
        {
          totalAmount.Write(fomcPlant[0].ToString() + '\t');
          totalAmount.Write((humcPlant[0] + romcPlant[0]).ToString() + '\t');
          totalAmount.Write(fomcManure[0].ToString() + '\t');
          totalAmount.Write((humcManure[0] + romcManure[0]).ToString() + '\t');
          totalAmount.Write(fomcPlantC14[0].ToString() + '\t');
          totalAmount.Write((humcPlantC14[0] + romcPlantC14[0]).ToString() + '\t');
          totalAmount.Write(fomcManureC14[0].ToString() + '\t');
          totalAmount.Write((humcManureC14[0] + romcManureC14[0]).ToString() + '\t');
          totalAmount.Write((fomcPlant[0] + humcPlant[0] + romcPlant[0] + fomcManure[0] + humcManure[0] + romcManure[0]).ToString() + '\t');
          
          totalAmount.Write(fomcPlant[1].ToString() + '\t');
          totalAmount.Write((humcPlant[1] + romcPlant[1]).ToString() + '\t');
          totalAmount.Write(fomcManure[1].ToString() + '\t');
          totalAmount.Write((humcManure[1] + romcManure[1]).ToString() + '\t');
          totalAmount.Write(fomcPlantC14[1].ToString() + '\t');
          totalAmount.Write((humcPlantC14[1] + romcPlantC14[1]).ToString() + '\t');
          totalAmount.Write(fomcManureC14[1].ToString() + '\t');
          totalAmount.Write((humcManureC14[1] + romcManureC14[1]).ToString() + '\t');
          totalAmount.Write((fomcPlant[1] + humcPlant[1] + romcPlant[1] + fomcManure[1] + humcManure[1] + romcManure[1]).ToString() + '\t');
          totalAmount.WriteLine((fomcPlant[0] + humcPlant[0] + romcPlant[0] + fomcManure[0] + humcManure[0] + romcManure[0] + fomcPlant[1] + humcPlant[1] + romcPlant[1] + fomcManure[1] + humcManure[1] + romcManure[1]).ToString() + '\t');
        }
        if (Mode.Value == 0 || Mode.Value == 1)
        {
          totalAmount.Write(fomcPlant[0].ToString() + '\t');
          totalAmount.Write(humcPlant[0].ToString() + '\t');
          totalAmount.Write(romcPlant[0].ToString() + '\t');
          
          totalAmount.Write(fomcManure[0].ToString() + '\t');
          totalAmount.Write(humcManure[0].ToString() + '\t');
          totalAmount.Write(romcManure[0].ToString() + '\t');
          if (Mode.Value == 0)
          {
            totalAmount.Write(fomcPlantC14[0].ToString() + '\t');
            totalAmount.Write(humcPlantC14[0].ToString() + '\t');
            totalAmount.Write(romcPlantC14[0].ToString() + '\t');
            
            totalAmount.Write(fomcManureC14[0].ToString() + '\t');
            totalAmount.Write(humcManureC14[0].ToString() + '\t');
            totalAmount.Write(romcManureC14[0].ToString() + '\t');
            totalAmount.Write(((fomcPlantC14[0] + humcPlantC14[0] + romcPlantC14[0] + fomcManureC14[0] + humcManureC14[0] + romcManureC14[0]) / (fomcPlant[0] + humcPlant[0] + romcPlant[0] + fomcManure[0] + humcManure[0] + romcManure[0]) * 100).ToString() + '\t');
          }
          totalAmount.Write((fomcPlant[0] + humcPlant[0] + romcPlant[0] + fomcManure[0] + humcManure[0] + romcManure[0]).ToString() + '\t');
          totalAmount.Write(fomcPlant[1].ToString() + '\t');
          totalAmount.Write(humcPlant[1].ToString() + '\t');
          totalAmount.Write(romcPlant[1].ToString() + '\t');
          
          totalAmount.Write(fomcManure[1].ToString() + '\t');
          totalAmount.Write(humcManure[1].ToString() + '\t');
          totalAmount.Write(romcManure[1].ToString() + '\t');
          if (Mode.Value == 0)
          {
            totalAmount.Write(fomcPlantC14[1].ToString() + '\t');
            totalAmount.Write(humcPlantC14[1].ToString() + '\t');
            totalAmount.Write(romcPlantC14[1].ToString() + '\t');
            
            totalAmount.Write(fomcManureC14[1].ToString() + '\t');
            totalAmount.Write(humcManureC14[1].ToString() + '\t');
            totalAmount.Write(romcManureC14[1].ToString() + '\t');
            totalAmount.Write(((fomcPlantC14[1] + humcPlantC14[1] + romcPlantC14[1] + fomcManureC14[1] + humcManureC14[1] + romcManureC14[1]) / (fomcPlant[1] + humcPlant[1] + romcPlant[1] + fomcManure[1] + humcManure[1] + romcManure[1]) * 100).ToString() + '\t');
          }
          
          totalAmount.WriteLine((fomcPlant[1] + humcPlant[1] + romcPlant[1] + fomcManure[1] + humcManure[1] + romcManure[1]).ToString() + '\t');
        }
        if (Mode.Value == 3)
        {
          DataRow row = totalAmountBack.NewRow();
          row[0] = fomcPlant[0];
          row[1] = humcPlant[0];
          row[2] = romcPlant[0];
          
          row[3] = fomcManure[0];
          row[4] = humcManure[0];
          row[5] = romcManure[0];
          
          row[6] = fomcPlantC14[0];
          row[7] = humcPlantC14[0];
          row[8] = romcPlantC14[0];
          
          row[9] = fomcManureC14[0];
          row[10] = humcManureC14[0];
          row[11] = romcManureC14[0];
          
          row[12] = ((fomcPlantC14[0] + humcPlantC14[0] + romcPlantC14[0] + fomcManureC14[0] + humcManureC14[0] + romcManureC14[0]) / (fomcPlant[0] + humcPlant[0] + romcPlant[0] + fomcManure[0] + humcManure[0] + romcManure[0]) * 100);
          row[13] = (fomcPlant[0] + humcPlant[0] + romcPlant[0] + fomcManure[0] + humcManure[0] + romcManure[0]);
          
          
          row[14] = fomcPlant[1];
          row[15] = humcPlant[1];
          row[16] = romcPlant[1];
          
          row[17] = fomcManure[1];
          row[18] = humcManure[1];
          row[19] = romcManure[1];
          
          row[20] = fomcPlantC14[1];
          row[21] = humcPlantC14[1];
          row[22] = romcPlantC14[1];
          
          row[23] = fomcManureC14[1];
          row[24] = humcManureC14[1];
          row[25] = romcManureC14[1];
          
          row[26] = ((fomcPlantC14[1] + humcPlantC14[1] + romcPlantC14[1] + fomcManureC14[1] + humcManureC14[1] + romcManureC14[1]) / (fomcPlant[1] + humcPlant[1] + romcPlant[1] + fomcManure[1] + humcManure[1] + romcManure[1]) * 100);
          row[27] = (fomcPlant[1] + humcPlant[1] + romcPlant[1] + fomcManure[1] + humcManure[1] + romcManure[1]);
          
          totalAmountBack.Rows.Add(row);
          DataRow rowCo2 = CO2Back.NewRow();
          rowCo2[0] = (co2FomPlant[0] + co2FomManure[0]);
          rowCo2[1] = (co2FomPlant[1] + co2FomManure[1]);
          rowCo2[2] = (co2HumPlant[0] + co2HumManure[0]);
          rowCo2[3] = (co2HumPlant[1] + co2HumManure[1]);
          rowCo2[4] = (co2RomPlant[0] + co2RomManure[0]);
          rowCo2[5] = (co2RomPlant[1] + co2RomManure[1]);
          CO2Back.Rows.Add(rowCo2);
          DataRow rowTransport = transportBack.NewRow();
          rowTransport[0] = (transportFomPlant + transportFomManure);
          rowTransport[1] = (transportHumPlant + transportHumManure);
          rowTransport[2] = (transportRomPlant + transportRomManure);
          transportBack.Rows.Add(rowTransport);
        }
        temperatureValuePosition++;
      }
    }
    
    // CLOSE streams
    if (Mode.Value == 0 || Mode.Value == 1)
    {
      totalAmount.Close();
      CO2.Close();
      transportFile.Close();
    }
    if (Mode.Value == 2)
    {
      totalAmount.Close();
      CO2.Close();
    }
    if (Mode.Value == 3)
    {
      returnSet.Tables.Add(transportBack);
      returnSet.Tables.Add(CO2Back);
      returnSet.Tables.Add(totalAmountBack);
      return returnSet;
    }
    
    return null;
  }
  
  double Rk4decay(double t0, double u0, double dt, double k, double tempCoefficient)
  {
    double f1 = func(t0, u0, k, tempCoefficient);
    double f2 = func(t0 + dt / 2, u0 + dt * f1 / 2, k, tempCoefficient);
    double f3 = func(t0 + dt / 2, u0 + dt * f2 / 2, k, tempCoefficient);
    double f4 = func(t0 + dt, u0 + dt * f3, k, tempCoefficient);
    
    double u1 = u0 + dt * (f1 + 2.0 * f2 + 2.0 * f3 + f4) / 6.0;
    return u1;
  }
  double func(double time, double amount, double k, double tempCoefficient)
  {
    double value = amount * -k * tempCoefficient;
    return value;
  }
  
  double TemperatureCoefficent(double temperature)
  {
    return 7.24 * Math.Exp(-3.432 + 0.168 * temperature * (1 - 0.5 * temperature / 36.9));
  }
  
  double Temperature(double depth)
  {
    double rho = Math.PI * 2.0 / (365.0 * 24.0 * 3600.0);
    double Th_diff = 0.35E-6;
    double dampingDepth = CalcDampingDepth(Th_diff, rho);
    double retVal = Temperatures.ValueAt(temperatureValuePosition) + Temperatures.Amplitude() * Math.Exp(-depth / dampingDepth) * Math.Sin(rho * (julianDay + Variables.Offset) * 24.0 * 3600.0 - depth / dampingDepth);
    return retVal;
  }
  
  double CalcDampingDepth(double k, double rho)
  {
    return Math.Sqrt(2.0 * k / rho);
  }
  
  private double DepthInLayer(int layer)
  {
    double depthInLayer;
    if (layer == 0)
      depthInLayer = 25.0 / 2.0;
    else
      depthInLayer = 75.0 / 2.0 + 25.0 / 2.0;
    return depthInLayer;
  }
  double[] DecompositionHum(double HUMdecompositionrate, ref double[] humc, double ROMfraction, ref double[] romc, bool WITH_TRANSPORT, bool C14, double DecayRate, ref double transportToFile)
  {
    double[] humcLeft = new double[NUMBER_OF_LAYERS];
    double[] CO2output = new double[NUMBER_OF_LAYERS];
    for (int j = 0; j < (NUMBER_OF_LAYERS); j++)
    {
      double depthInLayer = DepthInLayer(j);
      
      double tempCofficent = TemperatureCoefficent(Temperature(depthInLayer));
      
      double substract = 0;
      if (C14 == true)
        substract = DecayRate * humc[j];
      double humAfterDecom = Rk4decay(julianDay * t, humc[j], t, HUMdecompositionrate, tempCofficent) - substract;
      
      humcLeft[j] = humcLeft[j] + humAfterDecom;
      humc[j] = humc[j] - humAfterDecom;
      
      
      double CO2 = humc[j] * 0.628;
      CO2output[j] = CO2;
      double transport;
      if (WITH_TRANSPORT == true)
        transport = humc[j] * (1 - 0.628 - ROMfraction);
      else
        transport = 0;
      double romification = humc[j] * ROMfraction;
      humc[j] = humc[j] - CO2 - transport - romification;
      if (j != (NUMBER_OF_LAYERS - 1))
        humcLeft[j + 1] = humcLeft[j + 1] + transport;
      else
        humcLeft[j] = humcLeft[j] + transport;
      
      romc[j] = romc[j] + romification;
      if (j == 0)
        transportToFile = transport;
    }
    for (int j = 0; j < (NUMBER_OF_LAYERS); j++)
      humc[j] = humcLeft[j];
    return CO2output;
  }
  double[] DecompositionFom(ref double[] fomc, double FOMdecompositionrate, double tF, double humification, ref double[] humc, bool WITH_TRANSPORT, bool C14, double DecayRate, ref double transportFile)
  {
    double[] fomcLeft = new double[NUMBER_OF_LAYERS];
    double[] output = new double[4];
    double[] CO2output = new double[NUMBER_OF_LAYERS];
    for (int j = 0; j < NUMBER_OF_LAYERS; j++)
    {
      double depthInLayer = DepthInLayer(j);
      
      double substract = 0;
      if (C14 == true)
        substract = DecayRate * fomc[j];
      double tempCofficent = TemperatureCoefficent(Temperature(depthInLayer));
      
      double FomAfterDecom = Rk4decay(julianDay * t, fomc[j], t, FOMdecompositionrate, tempCofficent) - substract;
      
      fomcLeft[j] = fomcLeft[j] + FomAfterDecom;
      fomc[j] = fomc[j] - FomAfterDecom;
      double toLowerLayer;
      double inCorrentLayer;
      
      if (WITH_TRANSPORT == true)
      {
        toLowerLayer = fomc[j] * tF;
        inCorrentLayer = fomc[j] * (1 - tF);
      }
      else
      {
        toLowerLayer = 0;
        inCorrentLayer = 0;
      }
      if (j != (NUMBER_OF_LAYERS - 1))
        fomcLeft[j + 1] = fomcLeft[j + 1] + toLowerLayer;
      else
        fomcLeft[j] = fomcLeft[j] + toLowerLayer;
      
      fomc[j] = inCorrentLayer;
      double CO2 = fomc[j] * (1 - humification);
      double humificationAmount = fomc[j] * humification;
      fomc[j] = fomc[j] - CO2 - humificationAmount;
      humc[j] = humc[j] + humificationAmount;
      CO2output[j] = CO2;
      if (j == 0)
        transportFile = toLowerLayer;
    }
    for (int j = 0; j < NUMBER_OF_LAYERS; j++)
      fomc[j] = fomcLeft[j];
    
    return CO2output;
  }
  double[] DecompositionRom(ref double[] romc, double ROMdecompositionrate, bool WITH_TRANSPORT, bool C14, double DecayRate, ref double transportFile)
  {
    double[] romcLeft = new double[NUMBER_OF_LAYERS];
    double[] output = new double[4];
    double[] CO2output = new double[NUMBER_OF_LAYERS];
    for (int j = 0; j < (NUMBER_OF_LAYERS); j++)
    {
      double depthInLayer = DepthInLayer(j);
      
      double tempCofficent = TemperatureCoefficent(Temperature(depthInLayer));
      
      double substract = 0;
      if (C14 == true)
        substract = DecayRate * romc[j];
      double romAfterDecom = Rk4decay(julianDay * t, romc[j], t, ROMdecompositionrate, tempCofficent) - substract;
      
      romcLeft[j] = romcLeft[j] + romAfterDecom;
      double romcLeftForInert = romc[j] - romAfterDecom;
      romc[j] = romc[j] - romAfterDecom;
      double CO2 = romc[j] * 0.628;
      CO2output[j] = CO2;
      double transport;
      if (WITH_TRANSPORT == true)
        transport = romc[j] * (1 - 0.628);
      else
        transport = 0;
      
      romc[j] = romc[j] - CO2 - transport;
      
      if (j != (NUMBER_OF_LAYERS - 1))
        romcLeft[j + 1] = romcLeft[j + 1] + transport;
      else
        romcLeft[j] = romcLeft[j] + transport;
      
      transportFile = transport;
    }
    for (int j = 0; j < (NUMBER_OF_LAYERS); j++)
      romc[j] = romcLeft[j];
    return CO2output;
    
  }
  
  double R(double Clayfraction)
  {
    return 1.67 * (1.85 + 1.6 * Math.Exp(-7.86 * Clayfraction));
  }
}
}

