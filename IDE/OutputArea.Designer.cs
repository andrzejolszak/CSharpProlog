namespace Prolog
{
    partial class OutputArea
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
            this.tbInput = new System.Windows.Forms.TextBox();
            this.cbNewLines = new System.Windows.Forms.CheckBox();
            this.tbAnswer = new System.Windows.Forms.TextBox();
            this.btnClearA = new System.Windows.Forms.Button();
            this.btnEnter = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // tbInput
            // 
            this.tbInput.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.tbInput.BackColor = System.Drawing.Color.White;
            this.tbInput.Cursor = System.Windows.Forms.Cursors.IBeam;
            this.tbInput.Font = new System.Drawing.Font("Courier New", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.tbInput.Location = new System.Drawing.Point(4, 164);
            this.tbInput.Margin = new System.Windows.Forms.Padding(4);
            this.tbInput.Multiline = true;
            this.tbInput.Name = "tbInput";
            this.tbInput.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.tbInput.Size = new System.Drawing.Size(899, 43);
            this.tbInput.TabIndex = 31;
            this.tbInput.TabStop = false;
            this.tbInput.KeyDown += new System.Windows.Forms.KeyEventHandler(this.tbInput_KeyDown);
            // 
            // cbNewLines
            // 
            this.cbNewLines.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.cbNewLines.AutoSize = true;
            this.cbNewLines.Checked = true;
            this.cbNewLines.CheckState = System.Windows.Forms.CheckState.Checked;
            this.cbNewLines.Font = new System.Drawing.Font("Microsoft Sans Serif", 6.5F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.cbNewLines.Location = new System.Drawing.Point(909, 180);
            this.cbNewLines.Margin = new System.Windows.Forms.Padding(4);
            this.cbNewLines.Name = "cbNewLines";
            this.cbNewLines.Size = new System.Drawing.Size(68, 30);
            this.cbNewLines.TabIndex = 28;
            this.cbNewLines.TabStop = false;
            this.cbNewLines.Text = "Enter\r\nAccepts";
            this.cbNewLines.UseVisualStyleBackColor = true;
            this.cbNewLines.CheckedChanged += new System.EventHandler(this.cbNewLines_CheckedChanged);
            // 
            // tbAnswer
            // 
            this.tbAnswer.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.tbAnswer.BackColor = System.Drawing.Color.White;
            this.tbAnswer.Cursor = System.Windows.Forms.Cursors.Default;
            this.tbAnswer.Font = new System.Drawing.Font("Consolas", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.tbAnswer.Location = new System.Drawing.Point(4, 3);
            this.tbAnswer.Margin = new System.Windows.Forms.Padding(4);
            this.tbAnswer.Multiline = true;
            this.tbAnswer.Name = "tbAnswer";
            this.tbAnswer.ReadOnly = true;
            this.tbAnswer.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.tbAnswer.Size = new System.Drawing.Size(899, 156);
            this.tbAnswer.TabIndex = 27;
            this.tbAnswer.TabStop = false;
            // 
            // btnClearA
            // 
            this.btnClearA.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.btnClearA.BackColor = System.Drawing.SystemColors.Control;
            this.btnClearA.Image = global::Prolog.Properties.Resources.Symbols_Critical_32xSM;
            this.btnClearA.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnClearA.Location = new System.Drawing.Point(905, 3);
            this.btnClearA.Margin = new System.Windows.Forms.Padding(4);
            this.btnClearA.Name = "btnClearA";
            this.btnClearA.Size = new System.Drawing.Size(73, 31);
            this.btnClearA.TabIndex = 30;
            this.btnClearA.TabStop = false;
            this.btnClearA.Text = "Clear";
            this.btnClearA.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnClearA.UseVisualStyleBackColor = true;
            this.btnClearA.Click += new System.EventHandler(this.btnClearA_Click);
            // 
            // btnEnter
            // 
            this.btnEnter.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.btnEnter.Image = global::Prolog.Properties.Resources.StatusAnnotations_Complete_and_ok_32xSM_color;
            this.btnEnter.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnEnter.Location = new System.Drawing.Point(905, 147);
            this.btnEnter.Margin = new System.Windows.Forms.Padding(4);
            this.btnEnter.Name = "btnEnter";
            this.btnEnter.Size = new System.Drawing.Size(73, 32);
            this.btnEnter.TabIndex = 29;
            this.btnEnter.Text = "Enter";
            this.btnEnter.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnEnter.UseVisualStyleBackColor = true;
            this.btnEnter.Visible = false;
            this.btnEnter.Click += new System.EventHandler(this.btnEnter_Click);
            // 
            // OutputArea
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(981, 211);
            this.Controls.Add(this.tbInput);
            this.Controls.Add(this.btnClearA);
            this.Controls.Add(this.btnEnter);
            this.Controls.Add(this.cbNewLines);
            this.Controls.Add(this.tbAnswer);
            this.Font = new System.Drawing.Font("Microsoft Sans Serif", 7.8F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Name = "OutputArea";
            this.Text = "OutputArea";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        
        private System.Windows.Forms.Button btnClearA;
        private System.Windows.Forms.Button btnEnter;
        private System.Windows.Forms.CheckBox cbNewLines;
        public System.Windows.Forms.TextBox tbInput;
        public System.Windows.Forms.TextBox tbAnswer;
    }
}