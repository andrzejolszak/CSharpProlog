using System;
using System.Windows.Forms;
using Serilog;

namespace Prolog
{
    internal static class Program
    {
        /// <summary>
        ///     The main entry point for the application.
        /// </summary>
        [STAThread]
        private static void Main(string[] args)
        {
            Log.Logger = new LoggerConfiguration()
                .MinimumLevel.Information()
                .WriteTo.File("logs\\plw.log", rollingInterval: RollingInterval.Day)
                .CreateLogger();

            Log.Information("Starting CSProlog");

            AppDomain.CurrentDomain.UnhandledException += (x, y) =>
                Log.Error(((Exception)y.ExceptionObject).Message, (Exception)y.ExceptionObject);
            Application.ThreadException += (x, y) => Log.Error(y.Exception.Message, y.Exception);

            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new MainForm());
        }
    }
}