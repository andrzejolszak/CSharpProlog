namespace Prolog
{
    partial class DebuggerArea
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
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(DebuggerArea));
            this.callStackView = new System.Windows.Forms.TreeView();
            this.imageList1 = new System.Windows.Forms.ImageList(this.components);
            this.variablesView = new System.Windows.Forms.TreeView();
            this.debuggerPanel = new System.Windows.Forms.Panel();
            this.deuggerStepOutButton = new System.Windows.Forms.Button();
            this.debuggerStepIntoButton = new System.Windows.Forms.Button();
            this.debuggerStepOverButton = new System.Windows.Forms.Button();
            this.debuggerStopButton = new System.Windows.Forms.Button();
            this.debuggerContinueButton = new System.Windows.Forms.Button();
            this.debuggerPanel.SuspendLayout();
            this.SuspendLayout();
            // 
            // callStackView
            // 
            this.callStackView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.callStackView.ImageIndex = 0;
            this.callStackView.ImageList = this.imageList1;
            this.callStackView.Location = new System.Drawing.Point(4, 49);
            this.callStackView.Margin = new System.Windows.Forms.Padding(4);
            this.callStackView.Name = "callStackView";
            this.callStackView.SelectedImageIndex = 0;
            this.callStackView.ShowNodeToolTips = true;
            this.callStackView.Size = new System.Drawing.Size(643, 156);
            this.callStackView.TabIndex = 0;
            this.callStackView.NodeMouseDoubleClick += new System.Windows.Forms.TreeNodeMouseClickEventHandler(this.treeView1_NodeMouseDoubleClick);
            // 
            // imageList1
            // 
            this.imageList1.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("imageList1.ImageStream")));
            this.imageList1.TransparentColor = System.Drawing.Color.Transparent;
            this.imageList1.Images.SetKeyName(0, "ASCube_32xLG.png");
            this.imageList1.Images.SetKeyName(1, "test_32x_LG.png");
            this.imageList1.Images.SetKeyName(2, "color_wheel_32xLG.png");
            this.imageList1.Images.SetKeyName(3, "StatusAnnotations_Complete_and_ok_32xLG_color.png");
            this.imageList1.Images.SetKeyName(4, "StatusAnnotations_Critical_32xLG_color.png");
            // 
            // variablesView
            // 
            this.variablesView.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.variablesView.ImageIndex = 0;
            this.variablesView.ImageList = this.imageList1;
            this.variablesView.Location = new System.Drawing.Point(4, 213);
            this.variablesView.Margin = new System.Windows.Forms.Padding(4);
            this.variablesView.Name = "variablesView";
            this.variablesView.SelectedImageIndex = 0;
            this.variablesView.ShowNodeToolTips = true;
            this.variablesView.Size = new System.Drawing.Size(643, 172);
            this.variablesView.TabIndex = 1;
            // 
            // debuggerPanel
            // 
            this.debuggerPanel.Controls.Add(this.deuggerStepOutButton);
            this.debuggerPanel.Controls.Add(this.debuggerStepIntoButton);
            this.debuggerPanel.Controls.Add(this.debuggerStepOverButton);
            this.debuggerPanel.Controls.Add(this.debuggerStopButton);
            this.debuggerPanel.Controls.Add(this.debuggerContinueButton);
            this.debuggerPanel.Location = new System.Drawing.Point(4, 2);
            this.debuggerPanel.Margin = new System.Windows.Forms.Padding(4);
            this.debuggerPanel.Name = "debuggerPanel";
            this.debuggerPanel.Size = new System.Drawing.Size(503, 42);
            this.debuggerPanel.TabIndex = 34;
            this.debuggerPanel.Visible = false;
            // 
            // deuggerStepOutButton
            // 
            this.deuggerStepOutButton.Image = global::Prolog.Properties.Resources.Stepout_6327;
            this.deuggerStepOutButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.deuggerStepOutButton.Location = new System.Drawing.Point(401, 6);
            this.deuggerStepOutButton.Margin = new System.Windows.Forms.Padding(4);
            this.deuggerStepOutButton.Name = "deuggerStepOutButton";
            this.deuggerStepOutButton.Size = new System.Drawing.Size(96, 33);
            this.deuggerStepOutButton.TabIndex = 38;
            this.deuggerStepOutButton.TabStop = false;
            this.deuggerStepOutButton.Text = "Step Out F12";
            this.deuggerStepOutButton.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.deuggerStepOutButton.UseVisualStyleBackColor = true;
            this.deuggerStepOutButton.Click += new System.EventHandler(this.deuggerStepOutButton_Click);
            // 
            // debuggerStepIntoButton
            // 
            this.debuggerStepIntoButton.Image = global::Prolog.Properties.Resources.StepIn_6326;
            this.debuggerStepIntoButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.debuggerStepIntoButton.Location = new System.Drawing.Point(297, 6);
            this.debuggerStepIntoButton.Margin = new System.Windows.Forms.Padding(4);
            this.debuggerStepIntoButton.Name = "debuggerStepIntoButton";
            this.debuggerStepIntoButton.Size = new System.Drawing.Size(96, 33);
            this.debuggerStepIntoButton.TabIndex = 37;
            this.debuggerStepIntoButton.TabStop = false;
            this.debuggerStepIntoButton.Text = "Step Into F11";
            this.debuggerStepIntoButton.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.debuggerStepIntoButton.UseVisualStyleBackColor = true;
            this.debuggerStepIntoButton.Click += new System.EventHandler(this.debuggerStepIntoButton_Click);
            // 
            // debuggerStepOverButton
            // 
            this.debuggerStepOverButton.Image = global::Prolog.Properties.Resources.StepOver_6328;
            this.debuggerStepOverButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.debuggerStepOverButton.Location = new System.Drawing.Point(188, 6);
            this.debuggerStepOverButton.Margin = new System.Windows.Forms.Padding(4);
            this.debuggerStepOverButton.Name = "debuggerStepOverButton";
            this.debuggerStepOverButton.Size = new System.Drawing.Size(101, 33);
            this.debuggerStepOverButton.TabIndex = 36;
            this.debuggerStepOverButton.TabStop = false;
            this.debuggerStepOverButton.Text = "Step Over F10";
            this.debuggerStepOverButton.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.debuggerStepOverButton.UseVisualStyleBackColor = true;
            this.debuggerStepOverButton.Click += new System.EventHandler(this.debuggerStepOverButton_Click);
            // 
            // debuggerStopButton
            // 
            this.debuggerStopButton.Image = global::Prolog.Properties.Resources.Symbols_Stop_32xLG1;
            this.debuggerStopButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.debuggerStopButton.Location = new System.Drawing.Point(115, 6);
            this.debuggerStopButton.Margin = new System.Windows.Forms.Padding(4);
            this.debuggerStopButton.Name = "debuggerStopButton";
            this.debuggerStopButton.Size = new System.Drawing.Size(65, 33);
            this.debuggerStopButton.TabIndex = 35;
            this.debuggerStopButton.TabStop = false;
            this.debuggerStopButton.Text = "Stop";
            this.debuggerStopButton.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.debuggerStopButton.UseVisualStyleBackColor = true;
            this.debuggerStopButton.Click += new System.EventHandler(this.debuggerStopButton_Click);
            // 
            // debuggerContinueButton
            // 
            this.debuggerContinueButton.Image = global::Prolog.Properties.Resources.StatusAnnotations_Play_32xMD_color;
            this.debuggerContinueButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.debuggerContinueButton.Location = new System.Drawing.Point(4, 6);
            this.debuggerContinueButton.Margin = new System.Windows.Forms.Padding(4);
            this.debuggerContinueButton.Name = "debuggerContinueButton";
            this.debuggerContinueButton.Size = new System.Drawing.Size(103, 33);
            this.debuggerContinueButton.TabIndex = 34;
            this.debuggerContinueButton.TabStop = false;
            this.debuggerContinueButton.Text = "Continue F5";
            this.debuggerContinueButton.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.debuggerContinueButton.UseVisualStyleBackColor = true;
            this.debuggerContinueButton.Click += new System.EventHandler(this.debuggerContinueButton_Click);
            // 
            // DebuggerArea
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(650, 387);
            this.Controls.Add(this.debuggerPanel);
            this.Controls.Add(this.variablesView);
            this.Controls.Add(this.callStackView);
            this.Font = new System.Drawing.Font("Microsoft Sans Serif", 7.8F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Margin = new System.Windows.Forms.Padding(4);
            this.Name = "DebuggerArea";
            this.Text = "Debugger";
            this.debuggerPanel.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        

        private System.Windows.Forms.TreeView callStackView;
        private System.Windows.Forms.ImageList imageList1;
        private System.Windows.Forms.TreeView variablesView;
        private System.Windows.Forms.Panel debuggerPanel;
        private System.Windows.Forms.Button deuggerStepOutButton;
        private System.Windows.Forms.Button debuggerStepIntoButton;
        private System.Windows.Forms.Button debuggerStepOverButton;
        private System.Windows.Forms.Button debuggerStopButton;
        private System.Windows.Forms.Button debuggerContinueButton;
    }
}