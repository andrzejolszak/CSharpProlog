namespace Prolog
{
    partial class SourceArea
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        
        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.sourcePanel = new System.Windows.Forms.Panel();
            this.mapPanel = new System.Windows.Forms.Panel();
            this.bgwLoadSource = new System.ComponentModel.BackgroundWorker();
            this.consultButton = new System.Windows.Forms.Button();
            this.checkBox1 = new System.Windows.Forms.CheckBox();
            this.sourcePanel.SuspendLayout();
            this.SuspendLayout();
            // 
            // sourcePanel
            // 
            this.sourcePanel.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.sourcePanel.BackColor = System.Drawing.SystemColors.Control;
            this.sourcePanel.Controls.Add(this.mapPanel);
            this.sourcePanel.Location = new System.Drawing.Point(0, 53);
            this.sourcePanel.Margin = new System.Windows.Forms.Padding(4);
            this.sourcePanel.Name = "sourcePanel";
            this.sourcePanel.Size = new System.Drawing.Size(1183, 517);
            this.sourcePanel.TabIndex = 31;
            // 
            // mapPanel
            // 
            this.mapPanel.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.mapPanel.Location = new System.Drawing.Point(1147, 2);
            this.mapPanel.Margin = new System.Windows.Forms.Padding(4);
            this.mapPanel.Name = "mapPanel";
            this.mapPanel.Size = new System.Drawing.Size(33, 512);
            this.mapPanel.TabIndex = 33;
            // 
            // bgwLoadSource
            // 
            this.bgwLoadSource.WorkerReportsProgress = true;
            this.bgwLoadSource.WorkerSupportsCancellation = true;
            this.bgwLoadSource.DoWork += new System.ComponentModel.DoWorkEventHandler(this.bgwLoadSource_DoWork);
            this.bgwLoadSource.ProgressChanged += new System.ComponentModel.ProgressChangedEventHandler(this.bgwLoadSource_ProgressChanged);
            this.bgwLoadSource.RunWorkerCompleted += new System.ComponentModel.RunWorkerCompletedEventHandler(this.bgwLoadSource_RunWorkerCompleted);
            // 
            // consultButton
            // 
            this.consultButton.Image = global::Prolog.Properties.Resources.Deploy_dts_SQL_32xLG;
            this.consultButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.consultButton.Location = new System.Drawing.Point(5, 4);
            this.consultButton.Margin = new System.Windows.Forms.Padding(4);
            this.consultButton.Name = "consultButton";
            this.consultButton.Size = new System.Drawing.Size(98, 46);
            this.consultButton.TabIndex = 32;
            this.consultButton.TabStop = false;
            this.consultButton.Text = "Consult";
            this.consultButton.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.consultButton.UseVisualStyleBackColor = true;
            this.consultButton.Click += new System.EventHandler(this.consultButton_Click);
            // 
            // checkBox1
            // 
            this.checkBox1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.checkBox1.AutoSize = true;
            this.checkBox1.Checked = true;
            this.checkBox1.CheckState = System.Windows.Forms.CheckState.Checked;
            this.checkBox1.Location = new System.Drawing.Point(1059, 33);
            this.checkBox1.Name = "checkBox1";
            this.checkBox1.Size = new System.Drawing.Size(121, 17);
            this.checkBox1.TabIndex = 33;
            this.checkBox1.Text = "Draw dependencies";
            this.checkBox1.UseVisualStyleBackColor = true;
            this.checkBox1.CheckedChanged += new System.EventHandler(this.checkBox1_CheckedChanged);
            // 
            // SourceArea
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1183, 570);
            this.Controls.Add(this.checkBox1);
            this.Controls.Add(this.consultButton);
            this.Controls.Add(this.sourcePanel);
            this.Font = new System.Drawing.Font("Microsoft Sans Serif", 7.8F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Margin = new System.Windows.Forms.Padding(4);
            this.Name = "SourceArea";
            this.Text = "SourceArea";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.MainForm_FormClosing);
            this.sourcePanel.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        
        private System.Windows.Forms.Panel sourcePanel;
        private System.ComponentModel.BackgroundWorker bgwLoadSource;
        private System.Windows.Forms.Button consultButton;
        private System.Windows.Forms.Panel mapPanel;
        private System.Windows.Forms.CheckBox checkBox1;
    }
}